module Lcars.DHT.SimpleLocalnet (dhtLocalNetProcess) where

import Lcars.DHT
import Lcars.DHT.Types

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Platform
import Control.Distributed.Platform.GenServer as GenServer
import Control.Distributed.Platform.Timer as Timer



dhtLocalNetProcess :: Backend -> [NodeId] -> Process ()
dhtLocalNetProcess backend peers = do
  say "creating new DHT"
  dht <- newDHT
  say "starting DHT Server"
  (mainDhtServerId, mainDhtMonitor) <- startMonitor dht $ dhtServer
  rnd <- dhtRandomHash
  pid <- getSelfPid
  let p = DHTPutRequest 23 rnd pid
  say $ "sending out " ++ show p ++ " to " ++ show mainDhtServerId
--  resp <- (GenServer.call mainDhtServerId p) :: Process DHTResponse
--  say $ "show got " ++ show resp
  _ <- expect :: Process ()
  return ()
