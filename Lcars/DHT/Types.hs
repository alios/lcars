{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, TemplateHaskell #-}

module Lcars.DHT.Types ( DHTHash
                       , DHT(..)
                       , dhtLocalMapLength
                       , DHTConfig(..)
                       , defaultDHTConfig
                       , DHTCommand(..)
                       , DHTResponse(..)) where

import Control.Distributed.Platform.GenServer

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tagged
import Data.Typeable (Typeable)

import Data.DeriveTH

import qualified Data.Serialize as Ser
import Crypto.Types
import Crypto.Classes
import Crypto.Hash.Skein512 (Skein512)

type DHTHash = Skein512
type LocalDHT h = Map h ByteString
type PutID = DHTHash

data DHTCommand = 
  DHTPutRequest !Integer !PutID !ServerId |  
  DHTPut !PutID !ByteString
  deriving (Eq, Typeable, Show)

data DHTResponse = 
  DHTPutRequestAck !PutID |
  DHTPutDone !PutID !DHTHash
  deriving (Eq, Typeable, Show)

data DHT h = DHT {
  dhtId :: h,
  dhtConfig :: DHTConfig,
  dhtLocalMap :: LocalDHT h,
  dhtAllocators :: Map h DHTCommand
}

dhtLocalMapLength :: DHT h -> Integer
dhtLocalMapLength dht = 
  Map.foldl (\l bs -> l + (toInteger . BS.length $ bs)) 0 
  $ dhtLocalMap dht

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



$(derive makeBinary ''DHTCommand)
$(derive makeBinary ''DHTResponse)

