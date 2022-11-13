{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.IO.Class
import Database.PostgreSQL.PG
import Test.Hspec
import Database.PostgreSQL.Simple (connectPostgreSQL, Only (Only))
import Database.PostgreSQL.Simple.FromRow

data Person = Person {
  personId :: !Int,
  personLastName :: !String,
  personFirstName :: !String
} deriving (Show, Eq, Ord) 

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field 

prepareDatabase :: IO () 
prepareDatabase = do 
  conn <- connectPostgreSQL "dbname=postgres"
  withPG conn $ do
    execute_ "DROP DATABASE IF EXISTS person"
    execute_ "CREATE DATABASE person"
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

runTests :: IO ()
runTests = do
  conn <- connectPostgreSQL "dbname=person" 

  hspec $ do

    describe "execute_" $ 
      it "runs SQL without args and returns the number of rows affected" $ do
        rows <- withPG conn $
          execute_ "INSERT INTO person(first_name, last_name) VALUES('Carl', 'Schmitt')"
        rows `shouldBe` 1
    
    describe "execute" $
      it "runs SQL with args and returns the number of rows affected" $ do
        rows <- withPG conn $ do
          let args = ("Flim", "Flam") :: (String, String)
          execute "INSERT INTO person(first_name, last_name) VALUES(?, ?)" args
        rows `shouldBe` 1

    describe "query_" $
      it "queries SQL without args and returns the rows" $ do
        rows <- withPG conn $
          query_ "SELECT id, first_name, last_name FROM person ORDER BY last_name, first_name"
        rows `shouldBe` [Person 2 "Flim" "Flam", Person 1 "Carl" "Schmitt"]

    describe "query" $
      it "queries SQL with args and returns the rows" $ do
        rows <- withPG conn $ do
          let args = ("Flim", "Flam") :: (String, String)
          query "SELECT id, first_name, last_name FROM person WHERE first_name = ? and last_name = ?" args
        rows `shouldBe` [Person 2 "Flim" "Flam"]

    describe "query1_" $
      it "executes a query without args and returns the first row" $ do
        person <- withPG conn $
          query1_ "SELECT id, first_name, last_name FROM person WHERE id = 1"
        person `shouldBe` Person 1 "Carl" "Schmitt"
    
    describe "query" $
      it "executes a query with args and returns the first row" $ do
        person <- withPG conn $ do
          let arg = 2 :: Int
          query1 "SELECT id, first_name, last_name FROM person WHERE id = ?" (Only arg)
        person `shouldBe` Person 2 "Flim" "Flam"

    describe "value1_" $ 
      it "executes a query without args and returns the first value in the first row" $ do
        lastName :: String <- withPG conn $
          value1_ "SELECT last_name FROM person WHERE id = 1"
        lastName `shouldBe` "Schmitt"

    describe "value1" $ do
      it "executes a query with args and returns the first value in the first row" $ do
        lastName :: String <- withPG conn $ do
          let arg = 1 :: Int
          value1 "SELECT last_name FROM person WHERE id = ?" (Only arg)
        lastName `shouldBe` "Schmitt"

    describe "values_" $ do
      it "executes a query without args and returns the first value in all rows" $ do
        lastNames :: [String] <- withPG conn $
          values_ "SELECT last_name FROM person ORDER BY last_name"
        lastNames `shouldBe` ["Flam", "Schmitt"]

    describe "values" $ do
      it "executes a query with args and returns the first value in all rows" $ do
        lastNames :: [String] <- withPG conn $ do
          let arg = 0 :: Int
          values "SELECT last_name FROM person WHERE id > ? ORDER BY last_name" (Only arg)
        lastNames `shouldBe` ["Flam", "Schmitt"]

main :: IO ()
main = do 
  prepareDatabase
  runTests