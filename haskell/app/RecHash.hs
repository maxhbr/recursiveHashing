{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module RecHash where

import qualified Crypto.Hash.SHA256            as SHA256
import qualified Data.Aeson                    as A
import qualified Data.Aeson.Key                as K
import qualified Data.Aeson.KeyMap             as KM
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Base64        as B64
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.List                     as List
import qualified Data.Scientific               as Sc
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Vector                   as V

import Debug.Trace (trace)

class RecHash a where
  render :: a -> T.Text
  hash :: a -> T.Text
  hash a = let
      hashString :: T.Text -> T.Text
      hashString = T.decodeUtf8 . B64.encode . SHA256.hash . T.encodeUtf8
      rendered = render a
      hashed = hashString rendered
    in trace (show rendered ++ " -> " ++ show hashed) hashed

instance RecHash () where
  render _ = "null"

instance RecHash Bool where
  render = \case
    True  -> "true"
    False -> "false"

instance RecHash Sc.Scientific where
  render s = case Sc.floatingOrInteger s of
    Left  rational -> undefined -- TODO
    Right integral -> (T.pack . show) integral

instance RecHash T.Text where
  render t = "\"" `T.append` t `T.append` "\""

instance (RecHash a) => RecHash (V.Vector a) where
  render a = let
      hashes = map hash (V.toList a)
      combinedHashes = T.intercalate "," hashes
    in "[" `T.append` combinedHashes `T.append` "]"

data Member a = Member (K.Key, a)
instance (RecHash a) => RecHash (Member a) where
  render (Member (k, v)) =
    let vHash = hash v
    in (T.pack $ K.toString k) `T.append` ":" `T.append` vHash

instance (RecHash a) => RecHash (KM.KeyMap a) where
  render m = let
      hashes :: [T.Text]
      hashes = (map (hash . Member) . KM.toList) m
      sortedHashes = List.sort hashes
      combinedHashes = T.intercalate "," sortedHashes
    in "{" `T.append` combinedHashes `T.append` "}"

instance RecHash A.Value where
  render (A.Object o) = render o
  render (A.Array  a) = render a
  render (A.String t) = render t
  render (A.Number s) = render s
  render (A.Bool   b) = render b
  render (A.Null    ) = render ()

data JSONBS = JSONBS ByteString
instance RecHash JSONBS where
  render (JSONBS bs) = case (A.decode (BSL.fromStrict bs) :: Maybe A.Value) of
    Just decoded -> render decoded
    Nothing      -> undefined

renderJsonBS = render . JSONBS
hashJsonBS = hash . JSONBS
