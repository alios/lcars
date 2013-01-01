module Lcars.DHT ( DHT
                 , newDHT
                 , dhtPutServer
                 , dhtRandomHash
                 , dhtHash
                 , dhtHashStrict
                 , dhtPutRequest
                 ) where

import Lcars.DHT.Types
import Control.Concurrent.STM
import Control.Distributed.Process
import Control.Distributed.Platform.GenServer as GenServer
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


{-
dhtServer :: LocalServer (DHT DHTHash)
dhtServer = defaultServer {
  initHandler = do
     startMonitor 
     initOk Infinity
  , 
  handlers = []
  }
-}

newDHT :: IO (DHT DHTHash)
newDHT = do
  nodeid <- dhtRandomHash
  localMap <- newTMVarIO $ Map.empty
  return $ DHT nodeid defaultDHTConfig localMap Map.empty Map.empty


dhtRedistributeServer :: LocalServer (DHT DHTHash)
dhtRedistributeServer = defaultServer {
  handlers = [handle handleDHTCmdRequest]
  }

dhtPutServer :: LocalServer (DHT DHTHash)
dhtPutServer = defaultServer {
  handlers = [handle handleDHTCmdRequest]
  }

handleDHTCmdRequest :: Handler (DHT DHTHash) DHTCommand DHTResponse
handleDHTCmdRequest r@(DHTPutRequest l putid clientid) = do
  trace $ "handling DHTPutRequest: " ++ show r
  mapSizeIO <- fmap dhtLocalMapLength getState
  mapSize <- liftIO $ mapSizeIO
  maxMapSize <- fmap (dhtConfigMaxTableSize . dhtConfig) getState
  if (maxMapSize > 0 && mapSize + l >= maxMapSize) 
    then fail $ "requested " ++ show l ++ " bytes, only " ++ 
         show (maxMapSize - mapSize) ++ " are availible"
    else do modifyState $ \dht -> dht { 
              dhtAllocators = Map.insert putid r $ dhtAllocators dht 
              }
            
            ok $ DHTPutRequestAck putid

handleDHTCmdRequest r@(DHTPut putid bs) = do
  trace $ "handling DHTPut: " ++ show r
  let h = dhtHashStrict bs
  dhtInsertIO <- fmap (\dht -> dhtLocalMapInsert dht h bs) getState
  liftIO $ dhtInsertIO
  modifyState $ 
    \dht -> dht { 
      dhtAllocators = Map.delete putid $ dhtAllocators dht
      }
  ok $ DHTPutDone putid h


  