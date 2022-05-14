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

hashToStr :: ByteString -> T.Text
hashToStr = T.decodeUtf8 . B64.encode

wrapStr :: T.Text -> T.Text ->  T.Text
wrapStr "{" str = "{" `T.append` str `T.append` "}"
wrapStr "[" str = "[" `T.append` str `T.append` "]"
wrapStr  c  str = c `T.append` str `T.append` c

class RecHash a where
  recHash :: a -> ByteString
  recHashStr :: a -> T.Text
  recHashStr = hashToStr . recHash

instance RecHash ByteString where
  recHash = SHA256.hash

instance RecHash Bool where
  recHash = \case
    True  -> recHash ("true" :: ByteString)
    False -> recHash ("false" :: ByteString)

instance RecHash T.Text where
  recHash = recHash . T.encodeUtf8

instance RecHash Sc.Scientific where
  recHash s = case Sc.floatingOrInteger s of
    Left  rational -> undefined -- TODO
    Right integral -> (recHash . T.pack . show) integral

listOfHashesToStr :: T.Text -> [T.Text] -> T.Text
listOfHashesToStr c l = wrapStr c $ T.intercalate "," l

instance (RecHash a) => RecHash (V.Vector a) where
  recHash = recHash . listOfHashesToStr "[" . map recHashStr . V.toList

instance (RecHash a) => RecHash (KM.KeyMap a) where
  recHash = let
        memberToHashStr :: RecHash a => (K.Key, a) -> T.Text
        memberToHashStr (k, v) =
          let vHash = recHashStr v
          in  (T.pack $ K.toString k) `T.append` ":" `T.append` vHash
      in recHash . listOfHashesToStr "{" . List.sort . map memberToHashStr. KM.toList

instance RecHash A.Value where
    recHash (A.Object o) = recHash o
    recHash (A.Array  a) = recHash a
    recHash (A.String t) = recHash $ (wrapStr "\"" t)
    recHash (A.Number s) = recHash s
    recHash (A.Bool   b) = recHash b
    recHash (A.Null    ) = recHash ("null" :: ByteString)

recHashJsonBS :: ByteString -> Maybe T.Text
recHashJsonBS bs = case (A.decode (BSL.fromStrict bs) :: Maybe A.Value) of
  Just decoded -> Just $ recHashStr decoded
  Nothing      -> Nothing