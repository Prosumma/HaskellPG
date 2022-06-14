{-# LANGUAGE FlexibleContexts, GeneralisedNewtypeDeriving, OverloadedStrings #-}

module Main where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Data.ByteString (ByteString)
import Database.PostgreSQL.PG
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection, Only(..))
import Database.PostgreSQL.Simple.FromRow
import Data.String.Conversions
import Data.Time.Calendar (fromGregorian, Day)
import Data.Typeable 
import System.Environment

data Person = Person {
  personId :: Int,
  personName :: String,
  personDOB :: Day
} deriving Show

data NotConnectedException = NotConnectedException deriving (Show, Typeable) 
instance Exception NotConnectedException

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

getPeople :: MonadPG m => m [Person]
getPeople = query_ "SELECT id, name, dob FROM person ORDER BY dob"

getPerson :: (MonadThrow m, MonadPG m) => Int -> m Person 
getPerson id = query1 "SELECT id, name, dob FROM person WHERE id = ?" (Only id) 

addPerson :: (MonadThrow m, MonadPG m) => String -> Day -> m Person
addPerson name dob = query1 "\
  \INSERT INTO person(name, dob)\
  \SELECT ?, ?\
  \RETURNING id, name, dob\
\" (name, dob)

newtype App a = App { runApp :: StateT (Maybe Connection) IO a } 
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadState (Maybe Connection))

instance MonadConnection App where
  getConnection = App $ do 
    maybeConnection <- get 
    case maybeConnection of
      Just connection -> return connection
      _ -> throwM NotConnectedException

instance MonadPG App where
  interpret = interpg
  withTransaction transact = do
    (ret, conn) <- withPostgresTransaction $ 
      runStateT (runApp transact) . Just 
    put conn
    return ret

connectApp :: (MonadIO m, MonadState (Maybe Connection) m) => ByteString -> m ()
connectApp connectionString = do
  conn <- liftIO $ connectPostgreSQL connectionString 
  put $ Just conn 

app :: (MonadIO m, MonadState (Maybe Connection) m, MonadPG m, MonadThrow m) => ByteString -> m ()
app connectionString = do
  connectApp connectionString
  withTransaction $ getPeople >>= liftIO . print

main :: IO ()
main = do
  connectionString <- liftIO $ convertString <$> getEnv "PGCONNECTION"
  evalStateT (runApp (app connectionString)) Nothing 