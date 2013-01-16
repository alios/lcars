module Main (main) where 

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)

import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (threadDelay)

import Lcars


mainf :: [String] -> IO ()
mainf args = 
  case args of
    ["dhthost", host, port] -> do
      P2P.bootstrap host port [] $ do
        say $ "bootstrapping dhtLocalServer .. waiting to settle"
        liftIO $ threadDelay 1000000 
        let conf = defaultDHTConfig
        say $ "starting up DHT Server with conf: " ++ show conf
        serverid <- dhtStartLocalServer conf
        monref <- monitor serverid 
        say $ "init done " ++ show monref
        _ <- expect :: (Process ())
        say $ "unexpected response - terminating"
        exit serverid ()
        unmonitor monref
        terminate

main :: IO ()
main = do
  args <- getArgs
  mainf args

testmain = mainf [ "dhthost", "localhost", "9000" ]

