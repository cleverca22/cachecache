{-# LANGUAGE FlexibleInstances  #-}

module LogUtils (logMsg) where

import qualified Data.ByteString          as BS hiding (hPutStrLn, pack)
import qualified Data.ByteString.Char8    as BS (hPutStrLn, pack)
import           System.IO                (stderr)

class LogStream a where
  logMsg :: a -> IO ()

instance LogStream BS.ByteString where
  logMsg = BS.hPutStrLn stderr

instance LogStream String where
  logMsg msg = logMsg (BS.pack msg)

