{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import qualified Data.Maybe as M
import Data.Csv (encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy          as BSL

import RecHash

putTestData :: IO()
putTestData = let
    testDataJsons = [ "null"
                    , "true"
                    , "false"
                    , "123"
                    , "123e0"
                    , "123E0"
                    , "123e4"
                    , "123E4"
                    , "\"example\""
                    , "[]"
                    , "[123, \"456\"]"
                    , "[\"456\", 123]"
                    , "[{},[],null]"
                    , "{}"
                    , "{\"key1\": 123}"
                    , "{\"key1\": 123, \"key2\":[456, {\"key3\": true}]}"
                    ]
    testData = map (\j -> (j, hashJsonBS j, renderJsonBS j)) testDataJsons
    in BSL.writeFile "../testdata.csv" $ encode testData

generateTestdata = undefined


main :: IO ()
main = do
    args <- getArgs
    case args of 
        ["--gen-testdata"] -> putTestData
        files -> undefined
