{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances #-}

module Lcars.DHT.Servers ( DHTConfig(..), defaultDHTConfig
                         , DhtLocalServerState, dhtNewLocalServerState
                         , DHTCommandPut, DHTCommandPutResponse
                         , dhtLocalServer
                         , dhtStartLocalServerDefault, dhtStartLocalServer
                         ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable (Typeable)

import Control.Concurrent.STM
import Control.Distributed.Process.Platform.GenServer as GenServer
import Control.Distributed.Process

import Lcars.DHT.Classes
import Lcars.DHT.Types

deriving instance Typeable LcarsDHTHash

data DHTConfig = DHTConfig {
  dhtConfigMaxTableSize :: Integer
} deriving (Show, Eq)

defaultDHTConfig :: DHTConfig
defaultDHTConfig = DHTConfig 0

data DhtLocalServerState = MkDhtLocalServerState {
  dhtLocalServerStConfig :: DHTConfig,
  dhtDHT :: LcarsDHT,
  dhtLocalServerStPutServer :: TMVar (ServerId, MonitorRef)
}

dhtNewLocalServerStateDefault :: Process DhtLocalServerState
dhtNewLocalServerStateDefault = dhtNewLocalServerState defaultDHTConfig

{-
p :: ServerId -> LcarsDHTHash -> ByteString -> Process (Maybe h)
p n k v = do
  h <- undefined -- dhtRandomHash dht
  pid <- getSelfPid
  let putR
  GenServer.call n (MkDHTPutRequest (toInteger . BS.length $ v) h pid)
  -}

dhtNewLocalServerState :: DHTConfig -> Process DhtLocalServerState
dhtNewLocalServerState cfg = do
  let ctx = DhtFunctorCtx undefined
  dht <- dhtNewNode ctx
  putServer <- liftIO $ newEmptyTMVarIO
  say $ "created new server state"
  return $ MkDhtLocalServerState cfg dht putServer

data DhtLocalPutServerState = DhtLocalPutServerState {
  dhtLocalPutServerStParent :: DhtLocalServerState,
  dhtLocalPutServerStAllocators :: Map LcarsDHTHash DHTCommandPut
  }

dhtNewLocalPutServerState ::  
 DhtLocalServerState -> DhtLocalPutServerState
dhtNewLocalPutServerState lserv = DhtLocalPutServerState {
    dhtLocalPutServerStParent = lserv,
    dhtLocalPutServerStAllocators = Map.empty
  }


dhtLocalServer :: LocalServer DhtLocalServerState
dhtLocalServer = defaultServer
             
type DHTLocalPutServerHandler = 
    Handler DhtLocalPutServerState DHTCommandPut DHTCommandPutResponse

dhtLocalPutServer :: LocalServer DhtLocalPutServerState
dhtLocalPutServer = defaultServer {
  handlers = 
     let handlePut :: DHTLocalPutServerHandler
         handlePut r@(MkDHTPutRequest l putid clientid) = do
           trace $ "handling DHTPutRequest: " ++ show r
           dht <- fmap (dhtDHT . dhtLocalPutServerStParent) getState
--           mapSize <- lift $ dhtLocalMapSize dht -- TODO: get current map size
           --mapSize <- liftIO $ mapSizeIO
           let mapSize = 200000000000000000000
           maxMapSize <- fmap (dhtConfigMaxTableSize . 
                              dhtLocalServerStConfig . 
                              dhtLocalPutServerStParent) getState
           if (maxMapSize > 0 && mapSize + l >= maxMapSize) 
                 then fail $ "requested " ++ show l ++ " bytes, only " ++ 
                      show (maxMapSize - mapSize) ++ " are availible"
                 else do modifyState $ \dht -> dht { 
                         dhtLocalPutServerStAllocators = 
                           Map.insert putid r $ 
                             dhtLocalPutServerStAllocators dht }
                         ok $ MkDHTPutRequestAck putid

       in [handle $ handlePut]
}


dhtStartLocalServerDefault :: Process ServerId
dhtStartLocalServerDefault = dhtStartLocalServer defaultDHTConfig

dhtStartLocalServer :: DHTConfig -> Process ServerId
dhtStartLocalServer cfg = do
  say "starting up local DHT Server"
  servS <- dhtNewLocalServerState cfg
  localServerId <- startLink servS dhtLocalServer
  
  let putServS = dhtNewLocalPutServerState servS      
  putServ <- startMonitor putServS dhtLocalPutServer 
  liftIO $ atomically $ putTMVar
    (dhtLocalServerStPutServer servS) putServ
  say $ "server startup done - server id: " ++ show localServerId
  return localServerId
