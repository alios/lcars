{-# LANGUAGE TypeSynonymInstances, 
             DeriveDataTypeable, 
             TemplateHaskell, 
             MultiParamTypeClasses #-}

module Lcars.DHT.Types ( DHTHash
                       , dhtHashDistance
                       , DHT(..)
                       , dhtLocalMapLength
                       , dhtLocalMapInsert
                       , dhtLocalPutServerId
                       , DHTPut (..)
                       , dhtPeersCloser
                       , DHTConfig(..)
                       , defaultDHTConfig
                       , DHTPutCommand(..)
                       , DHTPutResponse(..)) where

import Control.Concurrent.STM
import Control.Distributed.Platform.GenServer
import Control.Distributed.Process
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Tagged
import Data.Typeable (Typeable)

import Data.DeriveTH

import qualified Data.Serialize as Ser
import Crypto.Types
import Crypto.Classes
import Crypto.Hash.Skein512 (Skein512)


type DHTHash = Skein512
type LocalDHT h = TMVar (Map h ByteString)
type PutID = DHTHash

data DHTPutCommand = 
  DHTPutRequest !Integer !PutID !ServerId |  
  DHTPut !PutID !ByteString
  deriving (Eq, Typeable, Show)

data DHTPutResponse = 
  DHTPutRequestAck !PutID |
  DHTPutDone !PutID !DHTHash
  deriving (Eq, Typeable, Show)

data DHT h = DHTState {
  dhtId :: h,
  dhtConfig :: DHTConfig,
  dhtLocalMap :: LocalDHT h,
  dhtPutServer :: TMVar (ServerId, MonitorRef)
}

data DHTPut h = DHTPutState {
  dhtPutParent :: DHT h,
  dhtPutAllocators :: Map h DHTPutCommand
}

data DHTPeers h = DHTPeersState {
  dhtPeersParent :: DHT h,
  dhtPeers :: Map h ServerId
}

dhtLocalPutServerId :: DHT h -> IO ServerId
dhtLocalPutServerId dht = fmap fst $ atomically $ 
                          readTMVar . dhtPutServer $ dht
  
dhtLocalMapLength :: DHT h -> IO Integer
dhtLocalMapLength dht = 
  let lenfold = Map.foldl (\l bs -> l + (toInteger . BS.length $ bs)) 0 
  in fmap lenfold $ atomically $ readTMVar $ dhtLocalMap dht

dhtLocalMapInsert :: DHT DHTHash -> DHTHash -> ByteString -> IO ()
dhtLocalMapInsert dht k v = atomically $ do
  let m' = dhtLocalMap dht
  m <- takeTMVar m'
  putTMVar m' $ Map.insert k v m

dhtPeersCloser :: DHTPeers DHTHash -> DHTHash -> [ServerId]  
dhtPeersCloser dht h = 
  let mydist = dhtHashDistance h $ dhtId $ dhtPeersParent dht
      ks = Map.keysSet . dhtPeers $ dht
      fkl = 
        Set.toList  $ Set.filter 
        (\k -> dhtHashDistance h k < mydist) ks
      sfkl = 
        List.sortBy 
        (\a b -> (dhtHashDistance h a)  `compare` 
                 (dhtHashDistance h b)) fkl
  in [ fromJust $ Map.lookup sk (dhtPeers dht) | sk <- sfkl ]


data DHTConfig = DHTConfig {
  dhtConfigMaxTableSize :: Integer
}

defaultDHTConfig :: DHTConfig
defaultDHTConfig = DHTConfig 0

dhtHashDistance :: DHTHash -> DHTHash -> Integer
dhtHashDistance a b = abs $ dhtHashToInteger a + dhtHashToInteger b

dhtHashFromInteger :: Integer -> DHTHash
dhtHashFromInteger i = 
  let bs = f (ol - 8) i
      ol = unTagged (outputLength :: Tagged DHTHash BitLength) 
      f l i = f' l i
      f' (-8) _ = [] 
      f' n i = (fromInteger ((i `shiftR` (n) ) .&. 0xff)) : f' (n-8) i
    in either error id $ Ser.decode $ BS.pack bs

dhtHashToInteger :: DHTHash -> Integer
dhtHashToInteger h =
  foldl (\i w -> (i `shiftL` 8) .|. toInteger w) (0 :: Integer) $ 
  BS.unpack $ Ser.encode h

instance Binary DHTHash where
  put = put . dhtHashToInteger
  get = fmap dhtHashFromInteger get


$(derive makeBinary ''DHTPutCommand)
$(derive makeBinary ''DHTPutResponse)

