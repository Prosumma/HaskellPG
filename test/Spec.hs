{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveAnyClass, DeriveGeneric #-}

import Database.PostgreSQL.PG
import Test.Hspec
import Database.PostgreSQL.Simple (connectPostgreSQL, close, Connection, Only (Only), ToRow, SqlError)
import Database.PostgreSQL.Simple.FromRow
import Data.List.Safe 
import GHC.Generics (Generic)

emptyListException :: Selector EmptyListException
emptyListException = const True

sqlError :: Selector SqlError
sqlError = const True

data Person = Person {
  personId :: !Int,
  personFirstName :: !String,
  personLastName :: !String
} deriving (Show, Eq, Ord)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

data NewPerson = NewPerson {
  newPersonFirstName :: !String,
  newPersonLastName :: !String
} deriving (Show, Eq, Ord, Generic, ToRow)

prepareDatabase :: IO Connection 
prepareDatabase = do
  conn <- connectPostgreSQL "dbname=postgres"
  withPG conn $ do
    execute_ "DROP DATABASE IF EXISTS person"
    execute_ "CREATE DATABASE person"
  close conn
  conn <- connectPostgreSQL "dbname=person"
  withPG conn $ do
    execute_ "\
      \CREATE TABLE person(\
        \id SERIAL NOT NULL PRIMARY KEY,\
        \first_name TEXT NOT NULL,\
        \last_name TEXT NOT NULL,\
        \UNIQUE(first_name, last_name)\
      \)\
    \"
  return conn

carl = Person 1 "Carl" "Schmitt"
murray = Person 2 "Murray" "Rothbard"

newCarl = NewPerson "Carl" "Schmitt"
newMurray = NewPerson "Murray" "Rothbard"
newMises = NewPerson "Ludwig" "von Mises"

runTests :: Connection -> IO ()
runTests conn = do
  hspec $ do

    describe "execute_" $ do
      it "runs SQL without args and returns the number of rows affected" $ 
        flip shouldReturn 1 $ 
          withPG conn $
            execute_ "INSERT INTO person(first_name, last_name) VALUES('Carl', 'Schmitt')"

    describe "execute" $
      it "runs SQL with args and returns the number of rows affected" $
        flip shouldReturn 1 $
          withPG conn $
            execute "INSERT INTO person(first_name, last_name) VALUES(?, ?)" newMurray 

    describe "query_" $
      it "queries SQL without args and returns the rows" $ do
        flip shouldReturn [murray, carl] $ do 
          withPG conn $ 
            query_ "SELECT id, first_name, last_name FROM person ORDER BY last_name, first_name"

    describe "query" $
      it "queries SQL with args and returns the rows" $ do
        flip shouldReturn [murray] $ do
          withPG conn $ do
            query "SELECT id, first_name, last_name FROM person WHERE first_name = ? and last_name = ?" newMurray 

    describe "query1_" $
      it "executes a query without args and returns the first row" $ do
        flip shouldReturn carl $ do
          withPG conn $
            query1_ "SELECT id, first_name, last_name FROM person WHERE id = 1"

    describe "query" $
      it "executes a query with args and returns the first row" $ do
        flip shouldReturn murray $
          withPG conn $ do
            let arg = 2 :: Int
            query1 "SELECT id, first_name, last_name FROM person WHERE id = ?" (Only arg)

    describe "value1_" $ do
      it "executes a query without args and returns the first value in the first row" $ do
        flip shouldReturn "Schmitt" $ do
          withPG conn $
            value1_ "SELECT last_name FROM person WHERE id = 1" :: IO String
      it "throws EmptyListException if no rows are returned" $
        flip shouldThrow emptyListException $ do
          withPG conn $
            value1_ "SELECT last_name FROM person WHERE id = 0" :: IO String

    describe "value1" $ do
      it "executes a query with args and returns the first value in the first row" $ do
        flip shouldReturn "Schmitt" $
          withPG conn $ do
            let arg = 1 :: Int
            value1 "SELECT last_name FROM person WHERE id = ?" (Only arg) :: PG String
      it "throws EmptyListException if no rows are returned" $ do
        flip shouldThrow emptyListException $ do
          let arg = 0 :: Int 
          withPG conn $ value1 "SELECT last_name FROM person WHERE id = ?" (Only arg) :: IO String

    describe "values_" $ do
      it "executes a query without args and returns the first value in all rows" $ do
        flip shouldReturn ["Rothbard", "Schmitt"] $ do
          withPG conn $ values_ "SELECT last_name FROM person ORDER BY last_name" :: IO [String]

    describe "values" $ do
      it "executes a query with args and returns the first value in all rows" $ do
        flip shouldReturn ["Rothbard", "Schmitt"] $ do
          withPG conn $ do
            let arg = 0 :: Int
            values "SELECT last_name FROM person WHERE id > ? ORDER BY last_name" (Only arg) :: PG [String] 

    describe "withTransaction" $ do
      it "rolls back a transaction if a failure occurs" $ do
        flip shouldThrow sqlError $ do
          withPG conn $ withTransaction $ do
            execute "INSERT INTO person(first_name, last_name) VALUES(?, ?)" newMises
            execute_ "rubbish"
        -- This proves that the insert attempted above failed.
        flip shouldReturn 2 $ do
          withPG conn $
            value1_ "SELECT COUNT(*) FROM person" :: IO Int
        flip shouldReturn 0 $ do
          withPG conn $
            value1 "SELECT COUNT(*) FROM person WHERE first_name = ? AND last_name = ?" newMises :: IO Int

main :: IO ()
main = prepareDatabase >>= runTests 