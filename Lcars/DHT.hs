module Lcars.DHT ( DHT
                 , dhtLocalProcess
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
  respReq <- (callNode $ DHTPutRequest l rnd pid) :: Process DHTPutResponse
  case (respReq) of
    DHTPutRequestAck putidReq -> do
      if (not $ putidReq == rnd) 
        then fail $ "expected DHTPutRequestAck for " ++ 
             show rnd ++ " received one for " ++ show putidReq
        else do
        respPut <- (callNode $ DHTPut putidReq bs) :: Process DHTPutResponse
        case (respPut) of
          DHTPutDone putidPut h -> do
            if (putidPut == rnd) 
              then return h
              else fail $ "expected DHTPutDone for " ++
                   show rnd ++ " received one for " ++ show putidPut
          r -> fail $ "unexpected response on DHTPut: " ++ show r
    r -> fail $ "unexpected response on DHTPutRequest: " ++ show r

dhtLocalProcess :: Process ServerId
dhtLocalProcess = do
  say "starting up local dht process"
  dhtS <- liftIO $ newDHT
  (localDhtServerId, localDhtMonitor) <- 
    startMonitor dhtS $ dhtLocalServer
  let dhtPutS = newDHTPut dhtS
  putSrv <- startMonitor dhtPutS $ dhtLocalPutServer
  liftIO $ atomically $ putTMVar (dhtPutServer dhtS) putSrv
  return localDhtServerId


newDHT :: IO (DHT DHTHash)
newDHT = do
  nodeid <- dhtRandomHash
  localMap <- newTMVarIO $ Map.empty
  putSrv <- newEmptyTMVarIO
  return $ DHTState nodeid defaultDHTConfig localMap putSrv

newDHTPut :: DHT h -> DHTPut h
newDHTPut dht = DHTPutState {
  dhtPutParent = dht,
  dhtPutAllocators = Map.empty
  }


dhtLocalServer :: LocalServer (DHT DHTHash)
dhtLocalServer = defaultServer {
  handlers = [handle $ handleLDHTPutCmdRequest ]
  }
  where handleLDHTPutCmdRequest :: Handler (DHT DHTHash) DHTPutCommand DHTPutResponse
        handleLDHTPutCmdRequest _ = do
          ps <- fmap dhtLocalPutServerId getState >>= liftIO
          GenServer.forward ps

dhtLocalPutServer :: LocalServer (DHTPut DHTHash)
dhtLocalPutServer = defaultServer {
  handlers = [handle handleDHTPutCmdRequest]
  }

handleDHTPutCmdRequest :: Handler (DHTPut DHTHash) DHTPutCommand DHTPutResponse
handleDHTPutCmdRequest r@(DHTPutRequest l putid clientid) = do
  trace $ "handling DHTPutRequest: " ++ show r
  mapSizeIO <- fmap (dhtLocalMapLength . dhtPutParent) getState
  mapSize <- liftIO $ mapSizeIO
  maxMapSize <- fmap (dhtConfigMaxTableSize . dhtConfig . dhtPutParent ) getState
  if (maxMapSize > 0 && mapSize + l >= maxMapSize) 
    then fail $ "requested " ++ show l ++ " bytes, only " ++ 
         show (maxMapSize - mapSize) ++ " are availible"
    else do modifyState $ \dht -> dht { 
              dhtPutAllocators = Map.insert putid r $ dhtPutAllocators dht 
              }
            
            ok $ DHTPutRequestAck putid

handleDHTPutCmdRequest r@(DHTPut putid bs) = do
  trace $ "handling DHTPut: " ++ show r
  let h = dhtHashStrict bs
  dhtInsertIO <- fmap (\st -> dhtLocalMapInsert (dhtPutParent st) h bs) getState
  liftIO $ dhtInsertIO
  modifyState $ 
    \dht -> dht { 
      dhtPutAllocators = Map.delete putid $ dhtPutAllocators dht
      }
  ok $ DHTPutDone putid h


  