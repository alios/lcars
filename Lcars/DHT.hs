module Lcars.DHT ( DHT
                 , newDHT
                 , dhtServer
                 , dhtRandomHash
                 , dhtHash
                 , dhtHashStrict
                 , dhtPutRequest
                 ) where

import Lcars.DHT.Types

import Control.Distributed.Process
import Control.Distributed.Platform.GenServer as GenServer
import Crypto.Classes
import Crypto.Types
import Crypto.Random.API
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Tagged
import Control.Monad.IO.Class

newDHT :: (MonadIO m, Functor m) => m (DHT DHTHash)
newDHT = do
  nodeid <- dhtRandomHash
  return $ DHT nodeid defaultDHTConfig Map.empty Map.empty

dhtRandomHash :: (MonadIO m, Functor m) => m DHTHash
dhtRandomHash = do 
  fmap dhtHashStrict $ liftIO $ getSystemEntropy l
  where l = (untag $ (blockLength :: Tagged DHTHash BitLength)) `div` 8


dhtHash :: BL.ByteString -> DHTHash
dhtHash = hash

dhtHashStrict :: ByteString -> DHTHash
dhtHashStrict = dhtHash . BL.pack . BS.unpack

dhtPutRequest :: ServerId -> ByteString -> Process DHTHash
dhtPutRequest n bs = do
  rnd <- dhtRandomHash
  pid <- getSelfPid
  let l = toInteger $ BS.length bs
  let callNode = GenServer.call n
  respReq <- (callNode $ DHTPutRequest l rnd pid) :: Process DHTResponse
  case (respReq) of
    DHTPutRequestAck putidReq -> do
      if (not $ putidReq == rnd) 
        then fail $ "expected DHTPutRequestAck for " ++ 
             show rnd ++ " received one for " ++ show putidReq
        else do
        respPut <- (callNode $ DHTPut putidReq bs) :: Process DHTResponse
        case (respPut) of
          DHTPutDone putidPut h -> do
            if (putidPut == rnd) 
              then return h
              else fail $ "expected DHTPutDone for " ++
                   show rnd ++ " received one for " ++ show putidPut
          r -> fail $ "unexpected response on DHTPut: " ++ show r
    r -> fail $ "unexpected response on DHTPutRequest: " ++ show r

handleDHTCmdRequest :: Handler (DHT DHTHash) DHTCommand DHTResponse

handleDHTCmdRequest r@(DHTPutRequest l putid clientid) = do
  trace $ "handling DHTPutRequest: " ++ show r
  mapSize <- fmap dhtLocalMapLength getState
  maxMapSize <- fmap (dhtConfigMaxTableSize . dhtConfig) getState
  if (maxMapSize > 0 && mapSize + l >= maxMapSize) 
    then fail $ "requested " ++ show l ++ " bytes, only " ++ show (maxMapSize - mapSize) ++ " are availible"
    else do modifyState $ \dht -> dht { 
              dhtAllocators = Map.insert putid r $ dhtAllocators dht 
              } 
            ok $ DHTPutRequestAck putid

handleDHTCmdRequest r@(DHTPut putid bs) = do
  trace $ "handling DHTPut: " ++ show r
  let h = dhtHashStrict bs
  modifyState $ 
    \dht -> dht { 
      dhtLocalMap = Map.insert h bs $ dhtLocalMap dht,
      dhtAllocators = Map.delete putid $ dhtAllocators dht
      }
  ok $ DHTPutDone putid h


dhtServer :: LocalServer (DHT DHTHash)
dhtServer = defaultServer {
  initHandler = dhtInitHandler,
  handlers = [handle handleDHTCmdRequest],
  terminateHandler = dhtTerminateHandler
  }

  
dhtInitHandler :: InitHandler (DHT DHTHash)
dhtInitHandler = do
  trace "dhtInitHandler"
  initOk Infinity

--dhtTerminateHandler :: TerminateReason -> Server (DHT DHTHash) ()
dhtTerminateHandler r = do
  trace $ "dhtTermianteHAndler reason: " ++ show r
  return ()