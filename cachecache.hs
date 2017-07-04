{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds           #-}

module Main where

import Data.Text
import           Data.Text       (Text, pack, stripSuffix)
import Data.Time (UTCTime)
import Servant.API
import Servant
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Time.Calendar
import GHC.Generics
import Data.Monoid ((<>))
import qualified Data.Text as T
import           Control.Monad
import           Network.Wreq (get, Response, responseStatus, statusCode, responseBody)
import           Flow
import qualified Data.ByteString.Lazy as LBS
import           Control.Lens
import           Control.Monad.Trans
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.HashMap.Strict as HM
import   Control.Concurrent.STM
import           Data.Typeable   (Typeable)
import           GHC.TypeLits
import           Control.Monad.ST
import           Control.Monad
import           Data.List
import           Debug.Trace

-- from https://github.com/haskell-servant/servant/pull/283
newtype Ext (ext :: Symbol) = Ext { getFileName :: Text } deriving (Typeable, Eq, Ord, Show)

instance (KnownSymbol ext) => FromHttpApiData (Ext ext) where
  parseUrlPiece   = parseUrlPiece >=> parseExt
  parseHeader     = parseHeader >=> parseExt
  parseQueryParam = parseQueryParam >=> parseExt

toExtProxy :: Ext ext -> Proxy ext
toExtProxy _ = Proxy

getExt :: KnownSymbol ext => Ext ext -> String
getExt t = symbolVal (toExtProxy t)

parseExt :: (KnownSymbol ext) => String -> Either Text (Ext ext)
parseExt str = res
  where
    res = case stripSuffix (pack ext) (pack str) of
      Nothing -> Left . pack $ "The filename \"" ++ str ++ "\" does not end with extension \"" ++ ext ++ "\""
      Just txt -> Right (Ext txt)
    ext = '.':getExt (toExtTy res)
    
    toExtTy :: Either Text (Ext ext) -> Ext ext
    toExtTy _ = undefined

data NarInfoRestriction = NarInfoRestriction { rawHash :: LBS.ByteString, textHash :: LT.Text } deriving (Eq, Ord, Show)
instance FromHttpApiData NarInfoRestriction where
  parseUrlPiece   = parseUrlPiece >=> parseNarInfoPath
  parseHeader     = parseHeader >=> parseNarInfoPath
  parseQueryParam = parseQueryParam >=> parseNarInfoPath
parseNarInfoPath :: String -> Either Text NarInfoRestriction
parseNarInfoPath str = res1
  where
    res1 = case LT.stripSuffix ".narinfo" (LT.pack str) of
      Nothing -> Left . pack $ "The file doesnt end in .narinfo"
      Just name -> res2 name
    res2 name = if (LT.length name == 32) then
        Right $ NarInfoRestriction undefined name
      else
        Left . pack $ "The hash length is not 32"

type Base32 = LBS.ByteString
parseHash32 :: LBS.ByteString -> ST s Base32
parseHash32 input = do
  let len = 32
  let base32Chars = "0123456789abcdfghijklmnpqrsvwxyz"
  when (LBS.length input /= len) $ fail "wrong hash length"
  forM_ [ 0 .. len - 1 ] $ \n -> do
    let
      c = LBS.index input n
      maybeIdx = LBS.elemIndex c base32Chars
      b = n * 5
      i = div b 8
      j = mod b 8
      -- i is the byte position
      -- j is the bit offset
      -- hash[i] = idx << j
    return undefined
    
    trace (show maybeIdx) return undefined
  return undefined

type NarInfoRequest = Capture "filename" NarInfoRestriction :> Get '[OctetStream] LBS.ByteString
type NarRequest = Capture "filename" (Ext "nar") :> Get '[OctetStream] LBS.ByteString
type TestRequest = "test" :> Capture "filename" NarInfoRestriction :> Get '[OctetStream] LBS.ByteString

type NarSubDir = "nar" :> Capture "filename" String :> Get '[OctetStream] LBS.ByteString

type RootDir = NarInfoRequest :<|> NarRequest :<|> NarSubDir

handleNarInfo :: TVar (HM.HashMap LT.Text LBS.ByteString) -> NarInfoRestriction -> Handler LBS.ByteString
handleNarInfo narinfo_cache hash = let
      filename = textHash hash
      checkNarInfo hash = do
          when (LT.length hash /= 32) $ throwError myerr
          rep <- lift $ (fetch narinfo_cache (NarInfo hash))
          handle rep
        where
          url = "https://cache.nixos.org/" <> hash <> ".narinfo"
          handle (Found bs) = return bs
      myerr = err500 { errBody = "invalid query" }
    in
      checkNarInfo filename

handleNar :: Ext "nar" -> Handler LBS.ByteString
handleNar file = return "hello world"

handleTest :: NarInfoRestriction -> Handler LBS.ByteString
handleTest file = do
  lift $ putStrLn $ show $ textHash file
  return "hello world"

handleNarSubDir :: TVar (HM.HashMap LT.Text LBS.ByteString) -> String -> Handler LBS.ByteString
handleNarSubDir narinfo_cache name = do
  res <- lift (fetch narinfo_cache $ Nar $ LT.pack $ "nar/" <> name)
  case res of
    Found lbs -> return lbs


server3 :: TVar (HM.HashMap LT.Text LBS.ByteString) -> Server RootDir
server3 narinfo_cache = (handleNarInfo narinfo_cache) :<|> handleNar :<|> (handleNarSubDir narinfo_cache)

userAPI :: Proxy RootDir
userAPI = Proxy

app1 :: TVar (HM.HashMap LT.Text LBS.ByteString)-> Application
app1 narinfo_cache = serve userAPI $ server3 narinfo_cache

main :: IO ()
main = do
  narinfo_cache <- newTVarIO $ HM.empty
  run 8081 $ app1 narinfo_cache

data RequestType = CacheInfo | NarInfo LT.Text | Nar LT.Text deriving Show
data ReplyType = Found LBS.ByteString | NotFound | ServerError deriving Show
fetch :: TVar (HM.HashMap LT.Text LBS.ByteString) -> RequestType -> IO ReplyType

fetch narinfo_cache (NarInfo hash) = do
  cache <- readTVarIO narinfo_cache
  case HM.lookup hash cache of
    (Just bs) -> return $ Found bs
    Nothing -> do
      reply <- fetch' (NarInfo hash)
      case reply of
        (Found bs) -> do
          atomically $ modifyTVar' narinfo_cache (HM.insert hash bs)
        _ -> return ()
      return reply

fetch _ (Nar path) = fetch' $ Nar path

fetch' :: RequestType -> IO ReplyType
fetch' CacheInfo = do
    req <- get url
    if req ^. responseStatus . statusCode == 200 then
        return $ Found $ req ^. responseBody
      else
        return NotFound
  where
    url = "https://cache.nixos.org/nix-cache-info"
fetch' (NarInfo hash) = do
    putStrLn "fetching"
    req <- get $ LT.unpack url
    if req ^. responseStatus . statusCode == 200 then
        return $ Found $ req ^. responseBody
      else
        return NotFound
  where
    url = "https://cache.nixos.org/" <> hash <> ".narinfo"

fetch' (Nar path) = do
    putStrLn $ "fetching " <> show path
    req <- get $ LT.unpack url
    if req ^. responseStatus . statusCode == 200 then
        return $ Found $ req ^. responseBody
      else
        return NotFound
  where
    url = "https://cache.nixos.org/" <> path
