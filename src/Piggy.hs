{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving #-}

module Piggy (
  Connection,
  MonadPG(..),
  PG(..),
  PGOperation(..),
  PGDSL,
  close,
  connectPG,
  connectPostgreSQL,
  execute,
  execute_,
  fromOnly,
  interpg,
  query,
  query_,
  query1,
  query1_,
  value1,
  value1_,
  values,
  values_,
  withConn1,
  withConn2,
  withPG,
  withPostgresTransaction
) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Operational
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.List.Safe (head)
import Database.PostgreSQL.Simple (close, connectPostgreSQL, fromOnly, Connection, Query, FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField 
import qualified Database.PostgreSQL.Simple as Postgres
import GHC.Int
import Prelude hiding (head)

-- | Abstractions representing an operation against a Postgres database. 
data PGOperation a where
  -- | Execute a query with argument(s) and return the number of rows affected.
  --
  -- This is used with SQL commands such as `UPDATE`, `DELETE`, and `INSERT` that
  -- do not return values.
  Execute :: ToRow q => Query -> q -> PGOperation Int64
  -- | Execute a query without arguments and return the number of rows affected.
  --
  -- This is used with SQL commands such as `UPDATE`, `DELETE`, and `INSERT` that
  -- do not return values.
  Execute_ :: Query -> PGOperation Int64 
  -- | Execute a query with arguments(s) and return the results mapped into
  -- the type `r`.
  Query :: (ToRow q, FromRow r) => Query -> q -> PGOperation [r]
  -- | Execute a query without arguments and return the results mapped into
  -- the type `r`.
  Query_ :: FromRow r => Query -> PGOperation [r]

-- | The DSL wrapped as needed for the Operational monad.
type PGDSL a = Program PGOperation a

-- | The Monad for types that provide a context for executing PostgreSQL commands.
--
-- This monad provides no facilities for connecting. That has to be handled
-- outside the scope of this monad.
class Monad m => MonadPG m where
  -- | The interpreter for the Postgres DSL
  --
  -- A default implementation — `interpg` — is provided, but other interpreters
  -- can be substituted in unit testing.
  interpret :: PGDSL a -> m a
  -- | Execute inside a Postgres transaction.
  --
  -- The default implementation does nothing. Use `withPostgresTransaction` in
  -- your own implementation to actually perform a Postgres transaction.
  withTransaction :: m a -> m a
  withTransaction t = t

class MonadConnection m where
  withConnection :: (Connection -> m a) -> m a

-- | Execute a function with the type `Connection -> a -> IO b`.
--
-- This is a helper function used in the implementation of `interpg`.
withConn1 :: (MonadConnection m, MonadIO m) => (Connection -> a -> IO b) -> a -> m b
withConn1 f a = withConnection $ \conn -> liftIO $ f conn a

-- | Execute a function with the type `Connection -> a -> b -> IO c`. 
--
-- This is a helper funtion used in the implementation of `interpg`.
withConn2 :: (MonadConnection m, MonadIO m) => (Connection -> a -> b -> IO c) -> a -> b -> m c
withConn2 f a b = withConnection $ \conn -> liftIO $ f conn a b

-- | Execute the given function inside a Postgres transaction.
--
-- Use this function to implement `MonadPG`'s `withTransaction`. For an example
-- implementation, see `PG`.
withPostgresTransaction :: (MonadConnection m, MonadUnliftIO m) => m a -> m a 
withPostgresTransaction action =  withConnection $ \conn -> 
  withRunInIO $ \run -> 
    Postgres.withTransaction conn $ run action

-- | The default interpreter for the PG DSL.
--
-- This interpreter actually talks to a Postgres database and calls Postgres functions.
-- Use this function to implement `MonadPG`'s `interpret`. For an example
-- implementation, see `PG`.
interpg :: (MonadConnection m, MonadIO m) => PGDSL a -> m a
interpg m = case view m of
  Return a -> return a
  (Execute sql q) :>>= k -> withConn2 Postgres.execute sql q >>= interpg . k
  (Execute_ sql) :>>= k -> withConn1 Postgres.execute_ sql >>= interpg . k 
  (Query sql q) :>>= k -> withConn2 Postgres.query sql q >>= interpg . k
  (Query_ sql) :>>= k -> withConn1 Postgres.query_ sql >>= interpg . k

-- | A Monad that implements `MonadPG` and `HasConnection` and talks to a Postgres database. 
newtype PG a = PG { runPG :: ReaderT Connection IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

-- | A helper function for running a `PG` if you've already got a `Connection`.
withPG :: MonadIO m => Connection -> PG a -> m a
withPG conn pg = liftIO $ runReaderT (runPG pg) conn

instance MonadConnection PG where
  withConnection pg = do 
    conn <- PG ask
    withPG conn (pg conn)

instance MonadPG PG where 
  interpret = interpg
  withTransaction = withPostgresTransaction 

instance MonadUnliftIO PG where
  withRunInIO action = PG $ withRunInIO $ \run -> action (run . runPG) 

-- | Connect to a Postgres database and run a `PG` instance with the given `Connection`.
connectPG :: MonadIO m => ByteString -> PG a -> m a
connectPG connectionString pg = liftIO $
  connectPostgreSQL connectionString >>= runReaderT (runPG pg)

-- | Execute a query with argument(s) and return the number of rows affected.
--
-- This is used with SQL commands such as `UPDATE`, `DELETE`, and `INSERT` that
-- do not return values.
execute :: (MonadPG m, ToRow q) => Query -> q -> m Int64
execute sql q = interpret $ singleton (Execute sql q)

-- | Execute a query without arguments and return the number of rows affected.
--
-- This is used with SQL commands such as `UPDATE`, `DELETE`, and `INSERT` that
-- do not return values.
execute_ :: MonadPG m => Query -> m Int64
execute_ sql = interpret $ singleton (Execute_ sql)

-- | Execute a query with arguments(s) and return the results mapped into
-- the type `r`.
query :: (MonadPG m, ToRow q, FromRow r) => Query -> q -> m [r]
query sql q = interpret $ singleton (Query sql q)

-- | Execute a query without arguments and return the results mapped into
-- the type `r`.
query_ :: (MonadPG m, FromRow r) => Query -> m [r]
query_ sql = interpret $ singleton (Query_ sql)

-- | Execute a query without arguments and return the first result.
--
-- Although it is not required, the SQL should include a `LIMIT` or
-- other expression. 
query1_ :: (MonadPG m, MonadThrow m, FromRow r) => Query -> m r
query1_ sql = query_ sql >>= head

-- | Execute a query with argument(s) and return the first result.
--
-- Although it is not required, the SQL should include a `LIMIT` or
-- other expression. 
query1 :: (MonadPG m, MonadThrow m, ToRow q, FromRow r) => Query -> q -> m r
query1 sql q = query sql q >>= head

-- | Execute a query with argument(s) and return the first column of the first result.
--
-- Although it is not required, the SQL should include a `LIMIT` or
-- other expression. 
value1 :: (MonadPG m, MonadThrow m, ToRow q, FromField v) => Query -> q -> m v
value1 sql q = fromOnly <$> query1 sql q

-- | Execute a query without arguments and return the first column of the first result.
--
-- Although it is not required, the SQL should include a `LIMIT` or
-- other expression. 
value1_ :: (MonadPG m, MonadThrow m, FromField v) => Query -> m v
value1_ sql = fromOnly <$> query1_ sql

-- | Execute a query with argument(s) and return the values in the first column.
values :: (MonadPG m, MonadThrow m, ToRow q, FromField v) => Query -> q -> m [v]
values sql q = map fromOnly <$> query sql q

-- | Execute a query without arguments and return the values in the first column.
values_ :: (MonadPG m, MonadThrow m, FromField v) => Query -> m [v]
values_ sql = map fromOnly <$> query_ sql
