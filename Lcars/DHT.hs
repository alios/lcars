module Lcars.DHT (dhtProcess, DHT) where

import Lcars.DHT.Types

import Control.Distributed.Process
import Control.Distributed.Platform
import Control.Distributed.Platform.GenServer

import Crypto.Classes
import Crypto.Random.API

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import qualified Data.Map as Map

import Control.Monad.IO.Class


dhtProcess :: [NodeId] -> Process ()
dhtProcess ns =
  let initState = undefined
  in do 
    mainDhtServer <- start initState dhtServer
    _ <- expect :: Process ()
    return ()

dhtServer :: LocalServer (DHT h)
dhtServer = defaultServer {
  initHandler = dhtInitHandler,
  handlers = []
  }

dhtInitHandler :: InitHandler (DHT h)
dhtInitHandler = do
  trace "dhtInitHandler"
  nodeid <- fmap hashStrict $ liftIO $ getSystemEntropy 4096
  trace $ "node id: " ++ show nodeid
  putState $ DHT (Map.empty, nodeid)
  initOk Infinity

hashStrict :: ByteString -> DHTHash
hashStrict = hash . BL.pack . BS.unpack

