{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds           #-}

module Main (main) where

import           Control.Concurrent.STM  (TVar, newTVarIO, readTVarIO, modifyTVar', atomically)
import           Control.Monad           (when, forM_, (>=>))
import           Control.Monad.ST        (ST)
import           Control.Monad.Trans     (lift)
import           Control.Lens            ((^.))
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text, pack, stripSuffix)
import qualified Data.Text.Lazy          as LT (Text, length, pack, unpack, stripSuffix)
import           Data.Monoid             ((<>))
import qualified Data.HashMap.Strict     as HM (HashMap, empty, lookup, insert)
import           Debug.Trace             (trace)
import           GHC.TypeLits            (KnownSymbol, Symbol, symbolVal)
import qualified Network.Wreq            as Wreq
import           Network.Wreq            (responseStatus, statusCode, responseBody, Response)
import           Network.Wai             (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant                 (FromHttpApiData, Server, errBody, Handler, OctetStream, Get, (:>), (:<|>) (..), Capture, parseHeader, parseQueryParam, parseUrlPiece, Proxy(Proxy), err404, err500, throwError, serve)

get :: LT.Text -> IO (Response LBS.ByteString)
get txt = Wreq.get $ LT.unpack txt

-- from https://github.com/haskell-servant/servant/pull/283
newtype Ext (ext :: Symbol) = Ext { getFileName :: Text } deriving (Eq, Ord, Show)

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
  let
    len = 32
    base32Chars :: LBS.ByteString
    base32Chars = "0123456789abcdfghijklmnpqrsvwxyz"
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
--type NarRequest = Capture "filename" (Ext "nar") :> Get '[OctetStream] LBS.ByteString

type NarSubDir = "nar" :> Capture "filename" String :> Get '[OctetStream] LBS.ByteString

type RootDir = NarInfoRequest  :<|> NarSubDir

handleNarInfo :: TVar (HM.HashMap LT.Text LBS.ByteString) -> NarInfoRestriction -> Handler LBS.ByteString
handleNarInfo narinfo_cache narinfo_req = do
    when (LT.length hash /= 32) $ throwError myerr
    rep <- lift $ (fetch narinfo_cache (NarInfo hash))
    case rep of
      Found bs -> return bs
      NotFound -> throwError $ err404 { errBody = "file not found" }
      ServerError -> throwError $ err500 { errBody = "internal error" }
  where
    hash = textHash narinfo_req
    myerr = err500 { errBody = "invalid query" }

handleNarSubDir :: TVar (HM.HashMap LT.Text LBS.ByteString) -> String -> Handler LBS.ByteString
handleNarSubDir narinfo_cache name = do
  res <- lift (fetch narinfo_cache $ Nar $ LT.pack $ "nar/" <> name)
  case res of
    Found lbs -> return lbs
    NotFound -> throwError $ err404 { errBody = "file not found" }
    ServerError -> throwError $ err500 { errBody = "internal error" }


server3 :: TVar (HM.HashMap LT.Text LBS.ByteString) -> Server RootDir
server3 narinfo_cache = (handleNarInfo narinfo_cache) :<|> (handleNarSubDir narinfo_cache)

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
fetch _ CacheInfo = fetch' CacheInfo

fetch' :: RequestType -> IO ReplyType
fetch' CacheInfo = do
  req <- get "https://cache.nixos.org/nix-cache-info"
  return $ if req ^. responseStatus . statusCode == 200
    then Found $ req ^. responseBody
    else NotFound

fetch' (NarInfo hash) = do
    putStrLn "fetching"
    req <- get $ "https://cache.nixos.org/" <>  hash <> ".narinfo"
    return $ if req ^. responseStatus . statusCode == 200
      then Found $ req ^. responseBody
      else NotFound

fetch' (Nar path) = do
    putStrLn $ "fetching NAR " <> show path
    req <- get $ "https://cache.nixos.org/" <> path
    return $ if req ^. responseStatus . statusCode == 200
      then Found $ req ^. responseBody
      else NotFound
