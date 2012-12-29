module Main (main) where 

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet

import Lcars



mainf ::[String] -> IO ()
mainf args = 
  case args of
    ["dhthost", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startMaster backend (dhtProcess)
{-    ["dhtput", host, port, key, value] -> do
      backend <- initializeBackend host port initRemoteTable
      let v = BS.pack . UTF8.encode $ value
      startMaster backend (dhtCmdPut backend key v)
    
-}

main :: IO ()
main = do
  args <- getArgs
  mainf args

testmain = mainf [ "dhthost", "localhost", "9000" ]