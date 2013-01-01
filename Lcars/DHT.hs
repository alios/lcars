module Lcars.DHT (DHT, newDHT, dhtServer, dhtRandomHash, dhtHash, dhtHashStrict) where

import Lcars.DHT.Types

import Control.Distributed.Process
import Control.Distributed.Platform.GenServer as GenServer
import Crypto.Classes
import Crypto.Random.API
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Control.Monad.IO.Class

newDHT :: (MonadIO m, Functor m) => m (DHT DHTHash)
newDHT = do
  nodeid <- dhtRandomHash
  return $ DHT nodeid Map.empty 


dhtRandomHash :: (MonadIO m, Functor m) => m DHTHash
dhtRandomHash = fmap dhtHashStrict $ liftIO $ getSystemEntropy 4096 

dhtHash :: BL.ByteString -> DHTHash
dhtHash = hash

dhtHashStrict :: ByteString -> DHTHash
dhtHashStrict = dhtHash . BL.pack . BS.unpack

dhtPutRequest :: ByteString -> ServerId -> Process DHTResponse
dhtPutRequest bs n = do
  rnd <- dhtRandomHash
  pid <- getSelfPid
  let l = toInteger $ BS.length bs
  let p = DHTPutRequest l rnd pid
  resp <- (GenServer.call n p) :: Process DHTResponse
  return resp

dhtServer :: LocalServer (DHT DHTHash)
dhtServer = defaultServer {
  initHandler = dhtInitHandler,
  handlers = [handle handleDHTCmdRequest],
  terminateHandler = dhtTerminateHandler
  }

handleDHTCmdRequest :: Handler (DHT DHTHash) DHTCommand DHTResponse
handleDHTCmdRequest r@(DHTPutRequest l putid clientid) = do
  trace $ "handling DHTPutRequest: " ++ show r
  ok $ DHTPutRequestAck putid
handleDHTCmdRequest r@(DHTPut putid bs) = do
  trace $ "handling DHTPut: " ++ show r
  let h = dhtHashStrict bs
  modifyState $ (\(DHT i dht) -> DHT i (Map.insert h bs dht))
  ok $ DHTPutDone putid h
  
dhtInitHandler :: InitHandler (DHT DHTHash)
dhtInitHandler = do
  trace "dhtInitHandler"
  initOk Infinity

--dhtTerminateHandler :: TerminateReason -> Server (DHT DHTHash) ()
dhtTerminateHandler r = do
  trace $ "dhtTermianteHAndler reason: " ++ show r
  return ()