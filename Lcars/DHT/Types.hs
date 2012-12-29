{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable #-}

module Lcars.DHT.Types (DHTHash, DHT(..)) where

import Data.Binary (Binary(..), Get, Put)
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import Data.Typeable (Typeable)

import qualified Data.Serialize as Ser

import Crypto.Hash.Skein512 (Skein512)

type DHTHash = Skein512
type LocalDHT h = Map h ByteString
type PutID = DHTHash

data DHTCommand = 
  DHTPutRequest Integer PutID  |
  DHTPut PutID ByteString 
  deriving (Eq, Typeable, Show)

newtype DHT h = DHT (LocalDHT h, DHTHash)

instance Binary DHTCommand where
  put (DHTPut putid bs) = do    
    Bin.put (0x24 :: Bin.Word8)
    Bin.put putid
    Bin.put $ BS.length bs
    Bin.put bs
  get = do
    magic <- Bin.getWord8
    if (not $ magic == 0x24) 
      then fail "magic byte 0x23 not found in parsing DHTCmd LcarsDHT"
      else do                      
        putid <- get :: Get PutID
        let putid = undefined
        l <- (get :: Get Int)
        bs <- Bin.getBytes l
        return $ DHTPut putid bs

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


