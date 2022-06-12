{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Database.PostgreSQL.PG where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Operational
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.List.Safe (head)
import Database.PostgreSQL.Simple (connectPostgreSQL, fromOnly, Connection, Query, FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField 
import qualified Database.PostgreSQL.Simple as Postgres
import GHC.Int
import Prelude hiding (head)

data PGOperation a where
  Execute :: ToRow q => Query -> q -> PGOperation Int64
  Execute_ :: Query -> PGOperation Int64 
  Query :: (ToRow q, FromRow r) => Query -> q -> PGOperation [r]
  Query_ :: FromRow r => Query -> PGOperation [r]

type PGDSL a = Program PGOperation a

class Monad m => MonadPG m where
  interpret :: PGDSL a -> m a
  withTransaction :: m a -> m a
  withTransaction t = t

class Monad m => MonadConnection m where
  getConnection :: m Connection

withConnection :: (MonadConnection m) => (Connection -> m a) -> m a
withConnection f = getConnection >>= f

withConn1 :: (MonadConnection m, MonadIO m) => (Connection -> a -> IO b) -> a -> m b
withConn1 f a = withConnection $ \conn -> liftIO $ f conn a

withConn2 :: (MonadConnection m, MonadIO m) => (Connection -> a -> b -> IO c) -> a -> b -> m c
withConn2 f a b = withConnection $ \conn -> liftIO $ f conn a b

interpg :: (MonadConnection m, MonadIO m) => PGDSL a -> m a
interpg m = case view m of
  Return a -> return a
  (Execute sql q) :>>= k -> withConn2 Postgres.execute sql q >>= interpg . k
  (Execute_ sql) :>>= k -> withConn1 Postgres.execute_ sql >>= interpg . k 
  (Query sql q) :>>= k -> withConn2 Postgres.query sql q >>= interpg . k
  (Query_ sql) :>>= k -> withConn1 Postgres.query_ sql >>= interpg . k

newtype PG a = PG { runPG :: ReaderT Connection IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

connectPG :: MonadIO m => ByteString -> PG a -> m a
connectPG connectionString pg = liftIO $ connectPostgreSQL connectionString >>= runReaderT (runPG pg) 

instance MonadPG PG where 
  interpret = interpg
  withTransaction t = withConnection $ \conn -> 
    liftIO $ Postgres.withTransaction conn $ runReaderT (runPG t) conn 

instance MonadConnection PG where
  getConnection = PG ask

execute :: (MonadPG m, ToRow q) => Query -> q -> m Int64
execute sql q = interpret $ singleton (Execute sql q)

execute_ :: MonadPG m => Query -> m Int64
execute_ sql = interpret $ singleton (Execute_ sql)

query :: (MonadPG m, ToRow q, FromRow r) => Query -> q -> m [r]
query sql q = interpret $ singleton (Query sql q)

query_ :: (MonadPG m, FromRow r) => Query -> m [r]
query_ sql = interpret $ singleton (Query_ sql)

query1_ :: (MonadPG m, MonadThrow m, FromRow r) => Query -> m r
query1_ sql = query_ sql >>= head

query1 :: (MonadPG m, MonadThrow m, ToRow q, FromRow r) => Query -> q -> m r
query1 sql q = query sql q >>= head

value1 :: (MonadPG m, MonadThrow m, ToRow q, FromField v) => Query -> q -> m v
value1 sql q = fromOnly <$> query1 sql q

value1_ :: (MonadPG m, MonadThrow m, FromField v) => Query -> m v
value1_ sql = fromOnly <$> query1_ sql