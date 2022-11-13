{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.IO.Class
import Database.PostgreSQL.PG
import Test.Hspec
import Test.Hspec.Expectations
import Database.PostgreSQL.Simple (connectPostgreSQL, close, Connection, Only (Only))
import Database.PostgreSQL.Simple.FromRow
import Data.List.Safe 

emptyListException :: Selector EmptyListException
emptyListException = const True

data Person = Person {
  personId :: !Int,
  personLastName :: !String,
  personFirstName :: !String
} deriving (Show, Eq, Ord)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

prepareDatabase :: IO Connection 
prepareDatabase = do
  conn <- connectPostgreSQL "dbname=postgres"
  withPG conn $ do
    execute_ "DROP DATABASE IF EXISTS person"
    execute_ "CREATE DATABASE person"
  close conn
  conn <- connectPostgreSQL "dbname=person"
  void $ withPG conn $ do
    execute_ "\
  \CREATE TABLE person(\
    \id SERIAL NOT NULL PRIMARY KEY,\
    \first_name TEXT NOT NULL,\
    \last_name TEXT NOT NULL,\
    \UNIQUE(first_name, last_name)\
  \)\
\"
  return conn

runTests :: Connection -> IO ()
runTests conn = do
  hspec $ do

    describe "execute_" $
      it "runs SQL without args and returns the number of rows affected" $ do
        flip shouldReturn 1 $ do
          withPG conn $
            execute_ "INSERT INTO person(first_name, last_name) VALUES('Carl', 'Schmitt')"

    describe "execute" $
      it "runs SQL with args and returns the number of rows affected" $ do
        flip shouldReturn 1 $ do
          withPG conn $ do
            let args = ("Flim", "Flam") :: (String, String)
            execute "INSERT INTO person(first_name, last_name) VALUES(?, ?)" args

    describe "query_" $
      it "queries SQL without args and returns the rows" $ do
        let expected = [Person 2 "Flim" "Flam", Person 1 "Carl" "Schmitt"]
        flip shouldReturn expected $ do 
          withPG conn $ 
            query_ "SELECT id, first_name, last_name FROM person ORDER BY last_name, first_name"

    describe "query" $
      it "queries SQL with args and returns the rows" $ do
        flip shouldReturn [Person 2 "Flim" "Flam"] $ do
          withPG conn $ do
            let args = ("Flim", "Flam") :: (String, String)
            query "SELECT id, first_name, last_name FROM person WHERE first_name = ? and last_name = ?" args

    describe "query1_" $
      it "executes a query without args and returns the first row" $ do
        flip shouldReturn (Person 1 "Carl" "Schmitt") $ do
          withPG conn $
            query1_ "SELECT id, first_name, last_name FROM person WHERE id = 1"

    describe "query" $
      it "executes a query with args and returns the first row" $ do
        flip shouldReturn (Person 2 "Flim" "Flam") $
          withPG conn $ do
            let arg = 2 :: Int
            query1 "SELECT id, first_name, last_name FROM person WHERE id = ?" (Only arg)

    describe "value1_" $ do
      it "executes a query without args and returns the first value in the first row" $ do
        flip shouldReturn "Schmitt" $ do
          withPG conn $ value1_ "SELECT last_name FROM person WHERE id = 1" :: IO String
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
        flip shouldReturn ["Flam", "Schmitt"] $ do
          withPG conn $ values_ "SELECT last_name FROM person ORDER BY last_name" :: IO [String]

    describe "values" $ do
      it "executes a query with args and returns the first value in all rows" $ do
        flip shouldReturn ["Flam", "Schmitt"] $ do
          withPG conn $ do
            let arg = 0 :: Int
            values "SELECT last_name FROM person WHERE id > ? ORDER BY last_name" (Only arg) :: PG [String] 

main :: IO ()
main = prepareDatabase >>= runTests 