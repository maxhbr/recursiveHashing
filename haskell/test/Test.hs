{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import           Data.ByteString                ( ByteString )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Aeson                    as A
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.Maybe                    as M


import Data.Aeson

import RecHash

data Prerendered = Prerendered T.Text
instance RecHash Prerendered where
    render (Prerendered t) = t
pureHashFunction' :: T.Text -> T.Text
pureHashFunction' = hash . Prerendered
pureHashFunction :: T.Text -> Maybe T.Text
pureHashFunction = Just . pureHashFunction'

main = do
    hspec $ do

        describe "assertions on hashing algorithm" $ do
            it "assert that empty string is hashed and base64 encoded correctly" $ do
                pureHashFunction' "" `shouldBe` "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
            it "assert that example string is hashed and base64 encoded correctly" $ do
                pureHashFunction' "example" `shouldBe` "UNhY4JhezH9gQYqvDMWrWH9CwlcKiECVqejMrND2VFw="

        describe "RecHash hashing should be recursive" $ do
            it "RecHash of: null" $ do
                hashJsonBS "null" `shouldBe` pureHashFunction "null"
                hashJsonBS " \n null  " `shouldBe` pureHashFunction "null"
            it "RecHash of: true" $ do
                hashJsonBS "true" `shouldBe` pureHashFunction "true"
                hashJsonBS " \n true  " `shouldBe` pureHashFunction "true"
            it "RecHash of: false" $ do
                hashJsonBS "false" `shouldBe` pureHashFunction "false"
                hashJsonBS " \n false  " `shouldBe` pureHashFunction "false"
                hashJsonBS "false" `shouldNotBe` hashJsonBS "true"
            it "RecHash of: numbers" $ do
                hashJsonBS "1223" `shouldBe` pureHashFunction "1223"
                hashJsonBS "1223e0" `shouldBe` pureHashFunction "1223"
                hashJsonBS "1223e3" `shouldBe` pureHashFunction "1223000"
                hashJsonBS "1223E3" `shouldBe` pureHashFunction "1223000"
                hashJsonBS " \n 1223e3  " `shouldBe` pureHashFunction "1223000"
            it "RecHash of: strings" $ do
                hashJsonBS "\"lorem ipsum\"" `shouldBe` pureHashFunction "\"lorem ipsum\""
                hashJsonBS " \n \"lorem ipsum\"  " `shouldBe` pureHashFunction "\"lorem ipsum\""
            it "RecHash of: empty arrays" $ do
                hashJsonBS "[]" `shouldBe` pureHashFunction "[]"
                hashJsonBS " \n [ \t  \n ]  " `shouldBe` pureHashFunction "[]"
            it "RecHash of: arrays" $ do
                hashJsonBS "[\"123\", 456]" `shouldBe` pureHashFunction ("[" `T.append` (pureHashFunction' "\"123\"") `T.append` "," `T.append` (pureHashFunction' "456") `T.append` "]")
            it "RecHash of: empty object" $ do
                hashJsonBS "{}" `shouldBe` pureHashFunction "{}"
                hashJsonBS " \n { \t  \n }  " `shouldBe` pureHashFunction "{}"

        describe "RecHash should respect array ordering" $ do
            it "RecHash should respect array ordering " $ do
                hashJsonBS "[\"value\", 456]" `shouldNotBe` hashJsonBS "[456, \"value\"]"

        describe "RecHash should ignore object ordering" $ do
            it "RecHash should ignore object ordering " $ do
                hashJsonBS "{ \"key1\": \"value\", \"key2\": 456}" `shouldBe` hashJsonBS "{ \"key2\": 456, \n\n\t \"key1\": \"value\"}"

        describe "RecHash should not overlap" $ do
            let empties = ["null", "\"null\"", "false", "true", "0", "\"\"", "[]", "{}", "{\"key\": null}"]
            mapM_ (\e1 -> mapM_ (\e2 -> do
                        it ("RecHash should not overlap on: " ++ show e1 ++ " <-> " ++ show e2) $ do
                            M.isJust (hashJsonBS e1) `shouldBe` True
                            hashJsonBS e1 `shouldNotBe` hashJsonBS e2
                        )  (filter (/= e1) empties)
                ) empties
