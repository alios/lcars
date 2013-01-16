{-# LANGUAGE MultiParamTypeClasses, 
             FlexibleInstances, 
             UndecidableInstances, 
             Rank2Types, 
             FlexibleContexts,
             TypeFamilies,
             DeriveDataTypeable
 #-}

module Lcars.DHT.Types ( DHT(..), LcarsDHTHash(..), LcarsDHT(..), DhtFunctorCtx(..)
                       , DHTCommandPut(..), DHTCommandPutResponse(..)
                       ) where

import Control.Concurrent.STM
import Data.List
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser
import Data.Tagged
import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Serialize as Ser
import Data.Typeable (Typeable)
import Crypto.Classes
import Crypto.Types
import Crypto.Random.API
import Crypto.Hash.Skein512 (Skein512)
import Control.Monad.IO.Class
import Control.Distributed.Process.Platform.GenServer
import Control.Distributed.Process

import Lcars.DHT.Classes

type LcarsDHTHash = Skein512
type LcarsDHT = DhtT LcarsDHTHash 

type TMHashMap h = TMVar (Map h ByteString)
type TMNodeMap h = TMVar (Map h (DHTNodeAddress (DhtT h)))

newtype DhtT h = DHT (h, TMHashMap h, TMNodeMap h, DhtFunctorCtx h)
              
mapDhtT :: (Functor f, MonadIO f) => 
           (Map t ProcessId -> b) -> DhtT t -> f b
mapDhtT f (DHT (h, _, nm, _)) = 
    fmap f $ liftIO  . atomically . readTMVar $ nm
    
instance (Hash ctx h) => DHT (DhtT h) h ByteString Process where
  type DHTHash (DhtT h) = h
  type DHTNodeAddress (DhtT h) = ServerId
  data DhtFunctorCtx h = DhtFunctorCtx {
    dhtPutFunctor :: DHTNodeAddress (DhtT h) -> DHTHash (DhtT h) -> ByteString -> Process (Maybe h)
  }

  dhtNodeId (DHT (h, _, _, _)) = h
  dhtFunctorContext (DHT (_, _, _, fctx)) = fctx
  dhtNodes = mapDhtT Map.keys
  dhtNodeAddress d k = mapDhtT (Map.lookup k) d
  dhtHash = hash . Data.Binary.encode
  dhtTryPutRemote ctx n k v = (dhtPutFunctor ctx) n k v 

  dhtRandomHash d = do 
    fmap (dhtHash) $ liftIO $ getSystemEntropy l
    where l = dhtHashOutputBytes d

  dhtNewNode fctx = do
    let dht = DHT undefined
    rid <- dhtRandomHash dht
    lm <- liftIO $ newTMVarIO Map.empty
    nm <- liftIO $ newTMVarIO Map.empty
    return $ DHT (rid, lm, nm, fctx)
    
  dhtLocalInsert (DHT (_, map', _, _)) k v = liftIO $ atomically $ do
    map <- readTMVar map'
    putTMVar map' $ Map.insert k v map

  dhtLocalDelete (DHT (_, map', _, _)) k = liftIO $ atomically $ do
    map <- readTMVar map'
    putTMVar map' $ Map.delete k map    

  dhtLocalGet (DHT (_, map', _, _)) k = 
     fmap (Map.lookup k) $ liftIO . atomically . readTMVar $ map'

  dhtHashOutputBits d = untag $ dhtTaggedOutputLength d
    where dhtTaggedOutputLength :: (Hash ctx h) => DhtT h -> Tagged h BitLength
          dhtTaggedOutputLength _ = outputLength



    
data DHTCommandPut = 
  MkDHTPutRequest !Integer !LcarsDHTHash !ServerId |  
  MkDHTPut !LcarsDHTHash !ByteString
  deriving (Eq, Typeable, Show)
                               
data DHTCommandPutResponse =
  MkDHTPutRequestAck !LcarsDHTHash |
  MkDHTPutDone !LcarsDHTHash !LcarsDHTHash
  deriving (Eq, Typeable, Show)

instance Binary (DHTCommandPutResponse) where
  put (MkDHTPutRequestAck h) = do 
    { put (0x42 :: Word8); put $ Ser.encode h }  
  put (MkDHTPutDone h1 h2) = 
    let sh1 = Ser.encode h1
        sh2 = Ser.encode h2
    in do  
      { put (0x43 :: Word8); put $ BS.length sh1 ; put sh1; 
        put $ BS.length sh2; put sh2 }  
  get = do
    magic <- getWord8
    case magic of 
      0x42 -> do
        h <- getRemainingSer
        return $ MkDHTPutRequestAck h
      0x43 -> do
        l1 <- get :: Get Int
        h1 <- getNSer l1
        l2 <- get :: Get Int
        h2 <- getNSer l2
        return $ MkDHTPutDone h1 h2

instance Binary DHTCommandPut where
  put (MkDHTPutRequest l h serverId) = do
    { put (0x23 :: Word8); put l; put serverId; put $ Ser.encode h }
  put (MkDHTPut h bs) = do
    { put (0x24 :: Word8); put $ BS.length bs; put bs; put $ Ser.encode h }
  get = do
    magic <- getWord8
    case magic of
      0x23 -> do
        l <- get
        serverId <- get
        r <- fmap (fromInteger . toInteger) remaining
        h <- getRemainingSer
        return $ MkDHTPutRequest l h serverId 
      0x24 -> do
        l <- get :: Get Int
        bs <- getBytes l
        h <- getRemainingSer
        return $ MkDHTPut h bs
  
getNSer :: Int -> (Ser.Serialize ser) => Get ser
getNSer n = do
  bs <- getLazyByteString $ fromInteger . toInteger $ n
  case (runGet getRemainingSer bs) of
    Left err -> fail err
    Right h -> return h
  
getRemainingSer :: (Ser.Serialize ser) => Get ser
getRemainingSer = do
  r <- fmap (fromInteger . toInteger) remaining
  h' <- fmap Ser.decode $ getByteString r
  case h' of
    Left err -> fail err
    Right h -> return h
