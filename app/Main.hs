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
  personLastName :: String,
  personFirstName :: String,
  personDOB :: Day
} deriving Show

data NotConnectedException = NotConnectedException deriving (Show, Typeable) 
instance Exception NotConnectedException

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field <*> field

clearPeople :: MonadPG m => m ()
clearPeople = void $ execute_ "DELETE FROM person"

getPeople :: MonadPG m => m [Person]
getPeople = query_ "SELECT id, last_name, first_name, dob FROM person ORDER BY dob"

getPerson :: (MonadThrow m, MonadPG m) => Int -> m Person 
getPerson id = query1 "SELECT id, last_name, first_name, dob FROM person WHERE id = ?" (Only id) 

addPerson :: (MonadThrow m, MonadPG m) => String -> String -> Day -> m Person
addPerson lastName firstName dob = query1 "\
  \INSERT INTO person(last_name, first_name, dob)\
  \SELECT ?, ?\
  \RETURNING id, last_name, first_name, dob\
\" (lastName, firstName, dob)

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
    put conn -- In case it got changed in `transact`.
    return ret

connectApp :: (MonadIO m, MonadState (Maybe Connection) m) => ByteString -> m ()
connectApp connectionString = do
  conn <- liftIO $ connectPostgreSQL connectionString 
  put $ Just conn 

app :: (MonadIO m, MonadState (Maybe Connection) m, MonadPG m, MonadThrow m) => ByteString -> m ()
app connectionString = do
  connectApp connectionString
  withTransaction $ do
    clearPeople
    addPerson "Flintstone" "Fred" (fromGregorian 0001 01 01)
    getPeople >>= liftIO . print

main :: IO ()
main = do
  connectionString <- liftIO $ convertString <$> getEnv "PGCONNECTION"
  evalStateT (runApp (app connectionString)) Nothing 