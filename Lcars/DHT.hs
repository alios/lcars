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
    (mainDhtServerId, mainDhtMonitor) <- startMonitor initState dhtServer
    _ <- expect :: Process ()
    return ()

dhtServer :: LocalServer (DHT DHTHash)
dhtServer = defaultServer {
  initHandler = dhtInitHandler,
  handlers = [handle handleDHTCmdRequest]
  }

handleDHTCmdRequest :: Handler (DHT DHTHash) DHTCommand DHTResponse
handleDHTCmdRequest r@(DHTPutRequest l putid) = do
  trace $ "handling DHTPutRequest: " ++ show r
  ok $ DHTPutRequestAck putid
handleDHTCmdRequest r@(DHTPut putid bs) = do
  trace $ "handling DHTPutRequest: " ++ show r
  let h = hashStrict bs
  modifyState $ (\(DHT (dht, i)) -> DHT (Map.insert h bs dht, i))
  ok $ DHTPutDone putid h
  
dhtInitHandler :: InitHandler (DHT h)
dhtInitHandler = do
  trace "dhtInitHandler"
  nodeid <- fmap hashStrict $ liftIO $ getSystemEntropy 4096
  trace $ "node id: " ++ show nodeid
  putState $ DHT (Map.empty, nodeid)
  initOk Infinity

hashStrict :: ByteString -> DHTHash
hashStrict = hash . BL.pack . BS.unpack

