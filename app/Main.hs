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
  \SELECT ?, ?, ?\
  \RETURNING id, last_name, first_name, dob\
\" (lastName, firstName, dob)

-- |
-- An example of a Monad that can talk to Postgres.
--
-- For more complex scenarios, something like `RWST Config () (Maybe Connection) IO a` may be a better choice. 
-- Of course, we can also create a Monad that requires a connection before it is run, so we'd just wrap `ReaderT`.
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
connectApp connectionString = liftIO (connectPostgreSQL connectionString) >>= put . Just 

disconnectApp :: (MonadState (Maybe Connection) m) => m ()
disconnectApp = put Nothing

app :: (MonadIO m, MonadState (Maybe Connection) m, MonadPG m, MonadThrow m) => ByteString -> m ()
app connectionString = do
  connectApp connectionString
  withTransaction $ do
    clearPeople
    addPerson "Flintstone" "Fred" (fromGregorian 0001 01 01)
    addPerson "Germanicus" "Gaius" (fromGregorian 0012 08 31)
    getPeople >>= liftIO . print
    disconnectApp

main :: IO ()
main = do
  connectionString <- liftIO $ convertString <$> getEnv "PGCONNECTION"
  evalStateT (runApp (app connectionString)) Nothing 