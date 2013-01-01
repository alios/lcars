module Lcars.DHT.SimpleLocalnet (dhtLocalNetProcess) where

import Lcars.DHT
import Lcars.DHT.Types

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Platform
import Control.Distributed.Platform.GenServer as GenServer
import Control.Distributed.Platform.Timer as Timer

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Binary as Bin


dhtLocalNetProcess :: Backend -> [NodeId] -> Process ()
dhtLocalNetProcess backend peers = do
  say "creating new DHT"
  dht <- dhtLocalProcess
  resp <- sequence . take 10 . repeat $ 
          dhtRandomTestPut dht
  say $ "show got " ++ show resp
  
  {-
  dht <- liftIO $ newDHT
  say "starting DHT Server"
  (mainDhtServerId, mainDhtMonitor) <- startMonitor dht $ dhtServer
  resp <- sequence . take 10 . repeat $ 
          dhtRandomTestPut mainDhtServerId
  say $ "show got " ++ show resp

  return ()
-}

dhtRandomTestPut serverId = do
  rnd <- dhtRandomHash
  dhtPutRequest serverId $ 
    BS.concat . BL.toChunks  . Bin.encode $ rnd