{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators      #-}

module Main (main) where

import           Control.Concurrent.STM
                 (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar,
                 readTVarIO)
import           Control.Lens             ((^.))
import           Control.Monad            (forM_, when, (>=>))
import           Control.Monad.ST         (ST)
import           Control.Monad.Trans      (lift)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.HashMap.Strict      as HM (HashMap, empty, insert, lookup)
import           Data.Monoid              ((<>))
import           Data.Text                (Text, pack, stripSuffix)
import qualified Data.Text.Lazy           as LT
                 (Text, length, pack, stripSuffix, unpack)
import           Debug.Trace              (trace)
import           GHC.TypeLits             (KnownSymbol, Symbol, symbolVal)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Network.Wreq
                 (Response, responseBody, responseStatus, statusCode)
import qualified Network.Wreq             as Wreq
import           Servant
                 ((:<|>) (..), (:>), Capture, FromHttpApiData, Get, Handler,
                 OctetStream, Proxy (Proxy), Server, err404, err500, errBody,
                 parseHeader, parseQueryParam, parseUrlPiece, serve,
                 throwError)

--------------------------------------------------------------------------------

data NarInfoRestriction
  = NarInfoRestriction
    { rawHash  :: LBS.ByteString
    , textHash :: LT.Text
    }
  deriving (Eq, Ord, Show)

instance FromHttpApiData NarInfoRestriction where
  parseUrlPiece   = parseUrlPiece >=> parseNarInfoPath
  parseHeader     = parseHeader >=> parseNarInfoPath
  parseQueryParam = parseQueryParam >=> parseNarInfoPath

--------------------------------------------------------------------------------

parseNarInfoPath :: String -> Either Text NarInfoRestriction
parseNarInfoPath str = res1
  where
    res1 = case LT.stripSuffix ".narinfo" (LT.pack str) of
      Nothing   -> Left . pack $ "The file doesnt end in .narinfo"
      Just name -> if (LT.length name == 32)
                    then Right $ NarInfoRestriction undefined name
                    else Left . pack $ "The hash length is not 32"

data CacheCache
  = CacheCache
    { narinfoCache :: !(TVar (HM.HashMap LT.Text LBS.ByteString))
  }

data RequestType
  = CacheInfo
  | NarInfo LT.Text
  | Nar LT.Text
  deriving Show
data ReplyType
  = Found LBS.ByteString
  | NotFound
  | ServerError
  deriving Show

get :: LT.Text -> IO (Response LBS.ByteString)
get txt = Wreq.get $ LT.unpack txt

lookupNarinfoCache :: CacheCache -> LT.Text -> STM (Maybe LBS.ByteString)
lookupNarinfoCache cache key = do
  hashmap <- readTVar (narinfoCache cache)
  return $ HM.lookup key hashmap

upsertNarinfoCache :: CacheCache -> LT.Text -> LBS.ByteString -> STM ()
upsertNarinfoCache cache key value = modifyTVar' (narinfoCache cache) (HM.insert key value)

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

handleNarInfo :: CacheCache -> NarInfoRestriction -> Handler LBS.ByteString
handleNarInfo cache narinfo_req = do
    when (LT.length hash /= 32) $ throwError myerr
    rep <- lift $ fetch cache $ NarInfo hash
    case rep of
      Found bs -> return bs
      NotFound -> throwError $ err404 { errBody = "file not found" }
      ServerError -> throwError $ err500 { errBody = "internal error" }
  where
    hash = textHash narinfo_req
    myerr = err500 { errBody = "invalid query" }

handleNarSubDir :: CacheCache -> String -> Handler LBS.ByteString
handleNarSubDir cache name = do
  res <- lift $ fetch cache $ Nar $ LT.pack $ "nar/" <> name
  case res of
    Found lbs -> return lbs
    NotFound -> throwError $ err404 { errBody = "file not found" }
    ServerError -> throwError $ err500 { errBody = "internal error" }


server3 :: CacheCache -> Server RootDir
server3 cache = (handleNarInfo cache) :<|> (handleNarSubDir cache)

userAPI :: Proxy RootDir
userAPI = Proxy

app1 :: CacheCache -> Application
app1 cache = serve userAPI $ server3 cache

main :: IO ()
main = do
  t1 <- newTVarIO $ HM.empty
  let cache = CacheCache t1
  run 8081 $ app1 cache

fetch :: CacheCache -> RequestType -> IO ReplyType

fetch cache (NarInfo hash) = do
  maybeRes <- atomically $ lookupNarinfoCache cache hash
  case maybeRes of
    (Just bs) -> return $ Found bs
    Nothing -> do
      reply <- fetch' (NarInfo hash)
      case reply of
        (Found bs) -> do
          atomically $ upsertNarinfoCache cache hash bs
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
