{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Text
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
import           Control.Monad (when)
import           Network.Wreq (get, Response, responseStatus, statusCode, responseBody)
import           Flow
import           Data.ByteString.Lazy (ByteString)
import           Control.Lens
import           Control.Monad.Trans
import qualified Data.Text.Encoding as T
import qualified Data.HashMap.Strict as HM
import   Control.Concurrent.STM

type NarInfo = "subdir" :> Capture "storehash" String :> Get '[OctetStream] ByteString
type UserAPI1 = "users" :> Get '[JSON] [User]

type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "one" :> Get '[JSON] User

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User


users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

one :: User
one = User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)


server1 :: Server UserAPI1
server1 = return users1

server2 :: Server UserAPI2
server2 = return users1 :<|> return one

handleNarInfo :: TVar (HM.HashMap Text ByteString)-> String -> Handler ByteString
handleNarInfo narinfo_cache filename = let
      fname = T.pack filename
      maybeHash = T.stripSuffix ".narinfo" fname
      checkNarInfo hash = do
          when (T.length hash /= 32) $ throwError myerr
          rep <- lift $ (fetch narinfo_cache (NarInfo hash))
          handle rep
        where
          url = "https://cache.nixos.org/" <> hash <> ".narinfo"
          handle (Found bs) = return bs
      myerr = err500 { errBody = "invalid query" }
    in
      maybe (throwError myerr) checkNarInfo maybeHash

server3 :: TVar (HM.HashMap Text ByteString)-> Server NarInfo
server3 narinfo_cache = handleNarInfo narinfo_cache

userAPI :: Proxy NarInfo
userAPI = Proxy

app1 :: TVar (HM.HashMap Text ByteString)-> Application
app1 narinfo_cache = serve userAPI $ server3 narinfo_cache

main :: IO ()
main = do
  narinfo_cache <- newTVarIO $ HM.empty
  run 8081 $ app1 narinfo_cache

data RequestType = CacheInfo | NarInfo Text | Nar deriving Show
data ReplyType = Found ByteString | NotFound | ServerError deriving Show
fetch :: TVar (HM.HashMap Text ByteString) -> RequestType -> IO ReplyType
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
    req <- get $ T.unpack url
    if req ^. responseStatus . statusCode == 200 then
        return $ Found $ req ^. responseBody
      else
        return NotFound
  where
    url = "https://cache.nixos.org/" <> hash <> ".narinfo"
