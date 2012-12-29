{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

module Lcars.DHT.Types (DHTHash, DHT(..), DHTCommand(..), DHTResponse(..)) where

import Control.Distributed.Platform.GenServer

import Data.Binary (Binary(..), Get, Put)
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Data (Data)
import Data.Map (Map)
import Data.Typeable (Typeable)

import qualified Data.Serialize as Ser

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


instance Binary DHTResponse where
  put (DHTPutRequestAck putid) = do
    put (0x42 :: Bin.Word8)
    put putid
  put (DHTPutDone putid h) = do
    put (0x43 :: Bin.Word8)
    put putid
    put h
  get = do
    magic <- Bin.getWord8
    case (magic) of 
      0x42 -> fmap DHTPutRequestAck get
      0x43 -> do 
        putid <- get
        h <- get
        return $ DHTPutDone putid h
        
instance Binary DHTCommand where
  put (DHTPutRequest l putid clientid) = do
    put (0x23 :: Bin.Word8)
    put l
    put putid
    put clientid
  put (DHTPut putid bs) = do    
    put (0x24 :: Bin.Word8)
    put putid
    put $ BS.length bs
    put bs
  get = do
    magic <- Bin.getWord8
    case (magic) of 
      0x23 -> do
        l <- get
        putid <- get
        clientid <- get
        return $ DHTPutRequest l putid clientid
      0x24 -> do                      
        putid <- get :: Get PutID
        let putid = undefined
        l <- (get :: Get Int)
        bs <- Bin.getBytes l
        return $ DHTPut putid bs
      i -> fail $ "unknown magic while parsing DHTCommand: " ++ show i
      

instance Binary DHTHash where
  put h =     
    let ser = Ser.encodeLazy h
    in do 
      put $ toInteger $ BL.length ser
      Bin.putLazyByteString ser
  get = do
    ml <- get
    case (ml) of 
      Left err -> fail $ "unable to parse hash length: " ++ err
      Right l  -> 
        do ser <- Bin.getLazyByteString $ fromInteger l
           case (Ser.decodeLazy ser) of
             Left err -> fail $ "unable to parse hash: " ++ err
             Right h -> return h


