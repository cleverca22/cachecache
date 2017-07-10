{-# LANGUAGE TypeApplications #-}

module Testing where

import qualified Pipes.Wai as WAI
import qualified Network.Wai as WAI
import           Control.Monad

foo :: WAI.Application
foo req respond = do
  let
    reqProd = WAI.producerRequestBody @IO req
    status = undefined
    headers= undefined
    responseProd = undefined
  respond (WAI.responseProducer status headers responseProd)
