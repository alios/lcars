{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, TemplateHaskell #-}

module Lcars.DHT.Types (DHTHash, DHT(..), DHTCommand(..), DHTResponse(..)) where

import Control.Distributed.Platform.GenServer

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
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
  dhtLocalMap :: LocalDHT h
}

$(derive makeBinary ''DHTCommand)
$(derive makeBinary ''DHTResponse)

instance Num DHTHash where
  a + b = fromInteger $ toInteger a + toInteger b
  a * b = fromInteger $ toInteger a * toInteger b
  abs = fromInteger . abs . toInteger
  signum = fromInteger . signum . toInteger
  fromInteger i = 
    let bs = f (ol - 8) i
        ol = unTagged (outputLength :: Tagged DHTHash BitLength) 
        f l i = f' l i
        f' (-8) _ = [] 
        f' n i = (fromInteger ((i `shiftR` (n) ) .&. 0xff)) : f' (n-8) i
    in either error id $ Ser.decode $ BS.pack bs
  
instance Enum DHTHash where
  fromEnum = fromEnum . toInteger
  toEnum = fromInteger . toEnum
  
instance Real DHTHash where
  toRational = toRational . toInteger
       
instance Integral DHTHash where
  toInteger h = 
    foldl (\i w -> (i `shiftL` 8) .|. toInteger w) (0 :: Integer) $ 
    BS.unpack $ Ser.encode h
  quotRem a b = 
    let (qi, ri) = quotRem (toInteger a) (toInteger b) 
    in (fromInteger qi, fromInteger ri)
  
instance Binary DHTHash where
  put h =     
    let ser = Ser.encodeLazy h
    in do 
      put $ toInteger $ BL.length ser
      putLazyByteString ser
  get = do
    ml <- get
    case (ml) of 
      Left err -> fail $ "unable to parse hash length: " ++ err
      Right l  -> 
        do ser <- getLazyByteString $ fromInteger l
           case (Ser.decodeLazy ser) of
             Left err -> fail $ "unable to parse hash: " ++ err
             Right h -> return h
