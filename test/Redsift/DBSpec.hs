{-# LANGUAGE QuasiQuotes #-}

module Redsift.DBSpec where

import Test.Hspec
import Redsift.DB
import Data.String.Interpolate.IsString

spec :: Spec
spec = do
  context "function on RsQuery" $ do
    describe "single query check" $ do
      it "is true for single line query without semicolon" $
        isSingleQuery (toRsQuery "select * from table") `shouldBe` True
      it "is true for single line query with semicolon" $
        isSingleQuery (toRsQuery "select * from table;") `shouldBe` True
      it "is true for multiple line query without semicolon" $ do
        let q = [i|
          SELECT *
          FROM table
        |]
        isSingleQuery (toRsQuery q) `shouldBe` True
      it "is true for multiple line query with 1 semicolon at the end" $ do
        let q = [i|
          SELECT *
          FROM table SELECT * FROM
          table2;|]
        isSingleQuery (toRsQuery q) `shouldBe` True
      it "is false for single line query with semicolon in the middle" $ do
        isSingleQuery (toRsQuery "SELECT * FROM table; SELECT * FROM table2") `shouldBe` False
      it "is false for multiple line query with semicolon in the middle" $ do
        let q = [i|
          SELECT *
          FROM table; SELECT * FROM
          table2
        |]
        isSingleQuery (toRsQuery q) `shouldBe` False

    describe "wrap limit around query" $ do
      let rsq = toRsQuery [i|
        SELECT *
        FROM table1;
      |]
          limitedQueryStrings = queryStrings $ limitQuery 100 rsq
      it "does wrap limit around the query" $ do
        limitedQueryStrings `shouldSatisfy` ((== "SELECT * FROM (") . head)
        limitedQueryStrings `shouldSatisfy` ((== ") LIMIT 100") . last)
      it "does remove the semicolon if the query has any" $ do
        limitedQueryStrings `shouldSatisfy` (not . any (';' `elem`))
