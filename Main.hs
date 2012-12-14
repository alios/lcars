module Main (main) where

import qualified Control.Distributed.Backend.P2P as P2P
import           Control.Monad.Trans (liftIO)
import           Control.Concurrent (threadDelay)

import Lcars (lcarsMain)

main :: IO ()
main = do    
  P2P.bootstrap "localhost" "9001" [P2P.makeNodeId "localhost:9000"] lcarsMain
  return ()