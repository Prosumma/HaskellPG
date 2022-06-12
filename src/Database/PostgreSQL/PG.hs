{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, OverloadedStrings, TypeFamilies #-}

module Database.PostgreSQL.PG where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Operational
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.List.Safe (head)
import Data.Proxy (Proxy(..))
import Database.PostgreSQL.Simple (connectPostgreSQL, fromOnly, Connection, Query, FromRow, ToRow)
import Database.PostgreSQL.Simple.FromField (FromField)
import qualified Database.PostgreSQL.Simple as Postgres
import GHC.Int (Int64)
import Prelude hiding (head)

data Instruction a where
  Execute :: ToRow q => Query -> q -> Instruction Int64
  Execute_ :: Query -> Instruction Int64 
  Query :: (ToRow q, FromRow r) => Query -> q -> Instruction [r]
  Query_ :: FromRow r => Query -> Instruction [r]

type PGDSL a = Program Instruction a

class Monad m => MonadPG m where
  type ConnectionType m
  getConnection :: m (ConnectionType m)
  interpretPG :: PGDSL a -> m a

withConnection :: MonadPG m => (ConnectionType m -> m a) -> m a
withConnection f = getConnection >>= f

withConn1 :: (MonadPG m, MonadIO m) => (ConnectionType m -> a -> IO b) -> a -> m b
withConn1 f a = withConnection $ \conn -> liftIO $ f conn a

withConn2 :: (MonadPG m, MonadIO m) => (ConnectionType m -> a -> b -> IO c) -> a -> b -> m c
withConn2 f a b = withConnection $ \conn -> liftIO $ f conn a b

interpg :: (MonadPG m, MonadIO m, ConnectionType m ~ Connection) => PGDSL a -> m a
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
  type ConnectionType PG = Connection
  getConnection = PG ask
  interpretPG = interpg

execute :: (MonadPG m, ToRow q) => Query -> q -> m Int64
execute sql q = interpretPG $ singleton (Execute sql q)

execute_ :: MonadPG m => Query -> m Int64
execute_ sql = interpretPG $ singleton (Execute_ sql)

query :: (MonadPG m, ToRow q, FromRow r) => Query -> q -> m [r]
query sql q = interpretPG $ singleton (Query sql q)

query_ :: (MonadPG m, FromRow r) => Query -> m [r]
query_ sql = interpretPG $ singleton (Query_ sql)

query1_ :: (MonadPG m, MonadThrow m, FromRow r) => Query -> m r
query1_ sql = query_ sql >>= head

query1 :: (MonadPG m, MonadThrow m, ToRow q, FromRow r) => Query -> q -> m r
query1 sql q = query sql q >>= head

value1 :: (MonadPG m, MonadThrow m, ToRow q, FromField v) => Query -> q -> m v
value1 sql q = fromOnly <$> query1 sql q

value1_ :: (MonadPG m, MonadThrow m, FromField v) => Query -> m v
value1_ sql = fromOnly <$> query1_ sql

getFoos :: MonadPG m => m [String]
getFoos = query_ "SELECT * FROM foo"