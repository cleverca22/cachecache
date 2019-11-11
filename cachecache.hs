{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NamedFieldPuns     #-}

module Main (main, rateToString) where

import           LogUtils                 (logMsg)
import           Types

import           Control.Concurrent.STM
                 (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar)
import           Control.Monad
import           Control.Monad.Trans      (liftIO)
import           Control.Exception        (catch)
import           Control.Concurrent.MVar  (MVar, putMVar, takeMVar, newEmptyMVar)
import qualified Data.ByteString          as BS hiding (hPutStrLn, pack)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.HashMap.Strict      as HM
                 (HashMap, delete, empty, insert, lookup)
import           Data.Monoid              ((<>))
import           Data.Text                (Text, pack, stripSuffix)
import qualified Data.Text.Lazy           as LT
                 (Text, length, pack, stripSuffix, unpack)
import qualified Data.Text.Lazy.Encoding  as LT
--import           Debug.Trace              (trace)
import           GHC.Conc                 (unsafeIOToSTM, forkIO, threadDelay, killThread)
import           GHC.TypeLits             (KnownSymbol, Symbol, symbolVal)
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (defaultSettings, setPort, runSettings, setTimeout)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Client      (newManager, Manager, managerResponseTimeout, httpLbs, host, path, defaultRequest, secure, port, Response, responseStatus, responseBody, responseTimeoutMicro)
import           Network.HTTP.Types.Status (statusCode)
import           Servant
                 ((:<|>) (..), (:>), Capture, FromHttpApiData, Get, Handler,
                 OctetStream, Proxy (Proxy), Server, err404, err500,
                 errBody, parseHeader, parseQueryParam, parseUrlPiece, serve,
                 throwError)

--import           Crypto.Hash.Algorithms   (SHA256)
--import qualified Pipes.ByteString         as PB
import           System.CPUTime           (getCPUTime)
--import           System.Posix.Signals     (installHandler, keyboardSignal)
--import qualified System.Posix.Signals     as Signals
import           System.TimeIt            (timeItT)
import           System.Directory         (doesDirectoryExist, createDirectory, doesFileExist)
--import qualified Testing                  as T
import           System.IO.Error          (isAlreadyInUseError)
import           Control.Monad.Extra      (unlessM)

import           System.Metrics.Prometheus.Concurrent.RegistryT
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import           System.Metrics.Prometheus.Http.Scrape
import           Control.Concurrent.Async
import           Control.Monad.Trans.Reader

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
      Just name -> if LT.length name == 32
                    then Right $ NarInfoRestriction undefined name
                    else Left . pack $ "The hash length is not 32"

data NarInfoCacheEntry
  = CacheHit { reply :: LBS.ByteString }
  | CacheMiss { timestamp :: Double }

data CacheStyle = CacheNarSub

data CacheCache
  = CacheCache
  { narinfoCache :: TVar (HM.HashMap LT.Text NarInfoCacheEntry)
  , upstreamCaches :: [ (Bool, BS.ByteString, Int, CacheStyle) ]
  , manager :: Manager
  , cacheMetrics :: Metrics
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

getNoThrow :: Manager -> (Bool, BS.ByteString, Int, LT.Text) -> IO (Response LBS.ByteString)
getNoThrow manager (secure, domain, port, url) = do
  let
    request = defaultRequest {
        host = domain
      , path = LBS.toStrict $ LT.encodeUtf8 url
      , secure = secure
      , port = port
    }
  response <- httpLbs request manager
  return response

lookupNarinfoCache :: CacheCache -> LT.Text -> STM (Maybe NarInfoCacheEntry)
lookupNarinfoCache cache key = do
  hashmap <- readTVar (narinfoCache cache)
  let
    value = HM.lookup key hashmap
  case value of
    Just (CacheMiss timestamp) -> do
      now <- unsafeIOToSTM getCPUTime
      let
        age = (fromIntegral now * 1e-12) - timestamp
      if age > 60 then do
        modifyTVar' (narinfoCache cache) (HM.delete key)
        return Nothing
      else
        return value
    _ -> return value


upsertNarinfoCache :: CacheCache -> LT.Text -> NarInfoCacheEntry -> STM ()
upsertNarinfoCache CacheCache{narinfoCache} key value = modifyTVar' narinfoCache $ HM.insert key value

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

type NarInfoRequest = Capture "filename" NarInfoRestriction :> Get '[OctetStream] LBS.ByteString
--type NarRequest = Capture "filename" (Ext "nar") :> Get '[OctetStream] LBS.ByteString

type NarSubDir = "nar" :> Capture "filename" String :> Get '[OctetStream] LBS.ByteString
type NixCacheInfo = "nix-cache-info" :> Get '[OctetStream] LBS.ByteString
type ShutdownUrl = "shutdown" :> Get '[OctetStream] LBS.ByteString
--type UserAPI1 = "users" :> Raw


handleNarInfo :: CacheCache -> NarInfoRestriction -> Handler LBS.ByteString
handleNarInfo cache@CacheCache{cacheMetrics} NarInfoRestriction{textHash} = do
  when (LT.length textHash /= 32) $ throwError $ err500 { errBody = "invalid query" }
  liftIO $ Counter.inc $ metricRequestsServed cacheMetrics
  rep <- liftIO $ fetch cache $ NarInfo textHash
  case rep of
    Found bs -> do
      liftIO $ Counter.inc $ metricInfoHit cacheMetrics
      return bs
    NotFound -> do
      liftIO $ Counter.inc $ metricInfoMiss cacheMetrics
      throwError $ err404 { errBody = "file not found" }
    ServerError -> throwError $ err500 { errBody = "internal error" }

handleNarSubDir :: CacheCache -> String -> Handler LBS.ByteString
handleNarSubDir cache@CacheCache{cacheMetrics} name = do
  liftIO $ Counter.inc $ metricRequestsServed cacheMetrics
  res <- liftIO $ fetch cache $ Nar $ LT.pack name
  case res of
    Found lbs -> do
      liftIO $ Counter.inc $ metricNarHit cacheMetrics
      return lbs
    NotFound -> do
      liftIO $ Counter.inc $ metricNarMiss cacheMetrics
      throwError $ err404 { errBody = "file not found" }
    ServerError -> throwError $ err500 { errBody = "internal error" }

handleNixCacheInfo :: Handler LBS.ByteString
handleNixCacheInfo = return "StoreDir: /nix/store\nWantMassQuery: 1\nPriority: 30\n"

shutdownHandler :: MVar () -> Handler LBS.ByteString
shutdownHandler shutdownMVar = do
  liftIO $ putMVar shutdownMVar ()
  return "done"

type RootDir = NarInfoRequest :<|> NarSubDir :<|> NixCacheInfo :<|> ShutdownUrl

server3 :: CacheCache -> MVar () -> Server RootDir
server3 cache shutdownMVar = (handleNarInfo cache) :<|> (handleNarSubDir cache) :<|> handleNixCacheInfo :<|> (shutdownHandler shutdownMVar)

userAPI :: Proxy RootDir
userAPI = Proxy

app1 :: CacheCache -> MVar () -> Application
app1 cache shutdownMVar = serve userAPI $ server3 cache shutdownMVar

startHelper :: CacheCache -> MVar () -> IO ()
startHelper cache shutdownMVar =
    catch (runSettings settings $ app1 cache shutdownMVar) handler
  where
    settings = setTimeout 600 $ setPort port $ defaultSettings
    port = 8081
    handler e = do
      logMsg $ "exception!: " <> show (e :: IOError)
      logMsg $ "t1: " <> show (isAlreadyInUseError e)
      if isAlreadyInUseError e
        then do
          _ <- getNoThrow (manager cache) (False, "localhost", port, "/shutdown")
          threadDelay $ 5 * 1000 * 1000
          startHelper cache shutdownMVar
        else putMVar shutdownMVar ()

registerMetricsServer :: IO (Metrics, Async ())
registerMetricsServer = runRegistryT $ do
  metrics <- makeMetrics
  registry <- RegistryT ask
  server <- liftIO . async $ runReaderT (unRegistryT $ serveHttpTextMetricsT 8080 []) registry
  pure (metrics, server)

makeMetrics :: RegistryT IO Metrics
makeMetrics = Metrics
  <$> registerCounter "requests_served" mempty
  <*> registerCounter "narinfo_hit" mempty
  <*> registerCounter "narinfo_miss" mempty
  <*> registerCounter "nar_hit" mempty
  <*> registerCounter "nar_miss" mempty
  <*> registerCounter "nar_bytes_hit" mempty
  <*> registerCounter "nar_bytes_miss" mempty

main :: IO ()
main = do
  (metrics, metricServer) <- registerMetricsServer
  t1 <- newTVarIO HM.empty
  shutdownMVar <- newEmptyMVar
  let
    customSettings = tlsManagerSettings { managerResponseTimeout = responseTimeoutMicro $ 60 * 1000000 }
  manager <- newManager customSettings
  let
    cache :: CacheCache
    cache = CacheCache t1 [
        (True, "cache.nixos.org", 443, CacheNarSub)
      , (True, "iohk-nix-cache.s3-eu-central-1.amazonaws.com", 443, CacheNarSub)
      ] manager metrics
  unlessM (doesDirectoryExist "cachedir") (createDirectory "cachedir")
  logMsg @String "started"
  child <- forkIO $ startHelper cache shutdownMVar
  takeMVar shutdownMVar
  logMsg @String "shutting down..."
  threadDelay $ 1000  * 1000
  logMsg @String "down"
  killThread child

fetch :: CacheCache -> RequestType -> IO ReplyType

fetch cache (NarInfo hash') = do
  (spent, maybeRes) <- timeItT $ atomically $ lookupNarinfoCache cache hash'
  logMsg $ "cache lookup took " <> show spent
  case maybeRes of
    (Just (CacheHit body)) -> do
      logMsg @String "cache hit hit"
      return $ Found body
    (Just (CacheMiss _)) -> do
      logMsg @String "cache hit miss"
      return NotFound
    Nothing -> do
      -- map' <- readTVarIO $ narinfoCache cache
      logMsg @String "cache miss" -- , cache size: " <> show (HM.size map')
      reply <- fetch' cache (NarInfo hash')
      case reply of
        Found bs ->
          atomically $ upsertNarinfoCache cache hash' $ CacheHit bs
        NotFound -> do
          now <- getCPUTime
          atomically $ upsertNarinfoCache cache hash' $ CacheMiss (fromIntegral now * 1e-12)
        _ -> return ()
      return reply

fetch cache (Nar path) = do
  hit <- doesFileExist (LT.unpack $ "cachedir/" <> path)
  if hit then do
    rawdata <- LBS.readFile (LT.unpack $ "cachedir/" <> path)
    Counter.add (fromIntegral $ LBS.length rawdata) (metricNarBytesHit . cacheMetrics $ cache)
    return $ Found rawdata
  else do
    result <- fetch' cache $ Nar path
    case result of
      Found rawdata -> do
        Counter.add (fromIntegral $ LBS.length rawdata) (metricNarBytesMiss . cacheMetrics $ cache)
        LBS.writeFile (LT.unpack $ "cachedir/" <> path) rawdata
      _ -> return ()
    return result

fetch cache CacheInfo = fetch' cache CacheInfo

rateToString :: Integral i => i -> Double -> String
rateToString size spent = do
  let
    rate = (fromIntegral size) / spent
    int2String :: Integer -> String
    int2String = show
    go :: Double -> String
    go x | x >= 0 && x <= 1024 = (int2String $ floor x) <> " bytes/second"
         | x <= (1024*1024) = (int2String $ floor (x/1024)) <> " kbytes/second"
         | otherwise = (int2String $ floor (x/1024/1024)) <> " mbytes/second"
  go rate

fetch' :: CacheCache -> RequestType -> IO ReplyType
fetch' cache CacheInfo = do
  req <- getNoThrow (manager cache) (True, "cache.nixos.org", 443, "/nix-cache-info")
  return $ if ((statusCode . responseStatus) req) == 200
    then Found $ responseBody req
    else NotFound

fetch' cache (NarInfo hash') = do
  let
    go :: [ (Bool, BS.ByteString, Int, CacheStyle) ] -> IO ReplyType
    go (next:rest) = do
      let
        (secure, domain, port, CacheNarSub) = next
      (spent, req) <- timeItT $ getNoThrow (manager cache) (secure, domain, port, "/" <> hash' <> ".narinfo")
      logMsg $ "fetching info " <> show hash' <> " took " <> show spent
      if (statusCode . responseStatus) req == 200
        then return $ Found $ responseBody req
        else go rest
    go [] = return NotFound
  req <- go (upstreamCaches cache)
  return req

fetch' cache (Nar path) = do
  let
    go :: [ (Bool, BS.ByteString, Int, CacheStyle) ] -> IO ReplyType
    go (next:rest) = do
      let
        (secure, domain, port, CacheNarSub) = next
      (spent, req) <- timeItT $ getNoThrow (manager cache) $ (secure, domain, port, "/nar/" <> path)
      let
        size = LBS.length $ responseBody req
      logMsg $ "fetching NAR " <> show path <> " took " <> show spent <> " rate " <> rateToString size spent
      if (statusCode . responseStatus) req == 200
        then return $ Found $ responseBody req
        else go rest
    go [] = return NotFound
  req <- go (upstreamCaches cache)
  return req

