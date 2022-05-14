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

pureHashFunction' :: ByteString -> T.Text
pureHashFunction' = hashToStr . recHash 

pureHashFunction :: ByteString -> Maybe T.Text
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
                recHashJsonBS "null" `shouldBe` pureHashFunction "null"
                recHashJsonBS " \n null  " `shouldBe` pureHashFunction "null"
            it "RecHash of: true" $ do
                recHashJsonBS "true" `shouldBe` pureHashFunction "true"
                recHashJsonBS " \n true  " `shouldBe` pureHashFunction "true"
            it "RecHash of: false" $ do
                recHashJsonBS "false" `shouldBe` pureHashFunction "false"
                recHashJsonBS " \n false  " `shouldBe` pureHashFunction "false"
                recHashJsonBS "false" `shouldNotBe` recHashJsonBS "true"
            it "RecHash of: numbers" $ do
                recHashJsonBS "1223" `shouldBe` pureHashFunction "1223"
                recHashJsonBS "1223e0" `shouldBe` pureHashFunction "1223"
                recHashJsonBS "1223e3" `shouldBe` pureHashFunction "1223000"
                recHashJsonBS "1223E3" `shouldBe` pureHashFunction "1223000"
                recHashJsonBS " \n 1223e3  " `shouldBe` pureHashFunction "1223000"
            it "RecHash of: strings" $ do
                recHashJsonBS "\"lorem ipsum\"" `shouldBe` pureHashFunction "\"lorem ipsum\""
                recHashJsonBS " \n \"lorem ipsum\"  " `shouldBe` pureHashFunction "\"lorem ipsum\""
            it "RecHash of: empty arrays" $ do
                recHashJsonBS "[]" `shouldBe` pureHashFunction "[]"
                recHashJsonBS " \n [ \t  \n ]  " `shouldBe` pureHashFunction "[]"
            it "RecHash of: arrays" $ do
                recHashJsonBS "[\"123\", 456]" `shouldBe` (pureHashFunction . T.encodeUtf8) ("[" `T.append` (pureHashFunction' "\"123\"") `T.append` "," `T.append` (pureHashFunction' "456") `T.append` "]")
            it "RecHash of: empty object" $ do
                recHashJsonBS "{}" `shouldBe` pureHashFunction "{}"
                recHashJsonBS " \n { \t  \n }  " `shouldBe` pureHashFunction "{}"

        describe "RecHash should respect array ordering" $ do
            it "RecHash should respect array ordering " $ do
                recHashJsonBS "[\"value\", 456]" `shouldNotBe` recHashJsonBS "[456, \"value\"]"

        describe "RecHash should ignore object ordering" $ do
            it "RecHash should ignore object ordering " $ do
                recHashJsonBS "{ \"key1\": \"value\", \"key2\": 456}" `shouldBe` recHashJsonBS "{ \"key2\": 456, \n\n\t \"key1\": \"value\"}"


        describe "RecHash should not overlap" $ do
            let empties = ["null", "\"null\"", "false", "true", "0", "\"\"", "[]", "{}", "{\"key\": null}"]
            mapM_ (\e1 -> mapM_ (\e2 -> do
                        it ("RecHash should not overlap on: " ++ show e1 ++ " <-> " ++ show e2) $ do
                            M.isJust (recHashJsonBS e1) `shouldBe` True
                            recHashJsonBS e1 `shouldNotBe` recHashJsonBS e2
                        )  (filter (/= e1) empties)
                ) empties
