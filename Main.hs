module Main (main) where

import Network.BSD
import qualified Control.Distributed.Backend.P2P as P2P
import Control.Monad.Trans (liftIO)
import Control.Concurrent (threadDelay)

import Lcars 

defaultPort1 = "9000"
defaultPort2 = "9001"
defaultPort3 = "9002"

main :: IO ()
main = do    
  hostName <- getHostName 
  let hid1 = hostName ++ ":" ++ defaultPort1
      hid2 = hostName ++ ":" ++ defaultPort2
      hid3 = hostName ++ ":" ++ defaultPort3
      domain = "alios.org"
      n1,n2,n3 :: LcarsNodeT
      n1 = defaultNode { lnName = "p1", lnDomain = domain } 
      n2 = defaultNode { lnName = "p2", lnDomain = domain } 
      n3 = defaultNode { lnName = "p3", lnDomain = domain } 
  P2P.bootstrap hostName defaultPort1 (map P2P.makeNodeId [ hid2, hid3 ]) $ nodeMain n1
  P2P.bootstrap hostName defaultPort2 (map P2P.makeNodeId [ hid1, hid3 ]) $ nodeMain n2
  P2P.bootstrap hostName defaultPort3 (map P2P.makeNodeId [ hid2, hid1 ]) $ nodeMain n3
  threadDelay 100000000
  return ()
  
  