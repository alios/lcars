module Lcars.DHT.SimpleLocalnet (dhtLocalNetProcess) where

import Lcars.DHT
import Lcars.DHT.Types

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Platform.GenServer as GenServer

dhtLocalNetProcess :: Backend -> [NodeId] -> Process ()
dhtLocalNetProcess backend peers = do
  dht <- newDHT
  (mainDhtServerId, mainDhtMonitor) <- startMonitor dht $ dhtServer
                                       
  rnd <- dhtRandomHash
  let p = DHTPutRequest 23 rnd mainDhtServerId
  cast mainDhtServerId p
  
  _ <- expect :: Process Int
  return ()
