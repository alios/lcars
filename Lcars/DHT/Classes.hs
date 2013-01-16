{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FunctionalDependencies, FlexibleContexts #-}

module Lcars.DHT.Classes (DHT(..)) where
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser
import Data.List

import Crypto.Types
import Control.Monad.IO.Class

class (MonadIO m, Functor m, Eq (DHTHash d), Ser.Serialize (DHTHash d)) => 
      DHT d h t m | h -> d, d -> h, d -> t, t -> d, d -> m where
  type DHTHash d :: *
  type DHTNodeAddress d :: *
  data DhtFunctorCtx h :: *
       
  dhtNewNode :: DhtFunctorCtx h -> m d
  dhtRandomHash :: d -> m h
  dhtFunctorContext :: d -> DhtFunctorCtx h
  dhtLocalInsert :: d -> DHTHash d -> t -> m ()
  dhtLocalGet :: d -> DHTHash d -> m (Maybe t)
  dhtLocalDelete :: d -> DHTHash d -> m ()
  dhtLocalMapSize :: d -> m Integer
  dhtNodeId :: d -> DHTHash d
  dhtNodeAddress :: d -> DHTHash d -> m (Maybe (DHTNodeAddress d))
  dhtPut :: d -> t -> m (Maybe (DHTHash d))
  dhtHash :: t -> (DHTHash d)
  dhtNodes :: d -> m [DHTHash d]
  dhtHashI :: d -> DHTHash d -> Integer
  dhtHashMax :: d -> Integer  
  dhtHashOutputBits :: d -> BitLength
  dhtHashOutputBytes :: d -> Int
  dhtDist :: d -> DHTHash d -> DHTHash d -> Integer
  dhtTryPutRemote :: DhtFunctorCtx h -> DHTNodeAddress d -> DHTHash d -> t -> m (Maybe (DHTHash d))
  dhtTryPutRemotes :: 
    d -> [DHTHash d] -> DHTHash d -> m (Maybe (DHTHash d))
  dhtTryPutRemotes d ns' h = 
    let ns = sortBy (\n1 n2 -> compare (hdist n1) (hdist n2)) ns'
        hdist = dhtDist d h
        fctx = dhtFunctorContext d
        putR _ [] _ _ = do return Nothing
        putR d (n:ns) h v = do
          let putRNext = putR d ns h v
          na' <- dhtNodeAddress d n 
          case na' of 
            Nothing -> putRNext
            Just na -> do 
              ret <- dhtTryPutRemote fctx na h v 
              case ret of
                Nothing -> putRNext
                Just hRet -> if (not $ hRet == h) 
                             then putRNext
                             else do
                               dhtLocalDelete d h
                               return $ Just h
    in do 
      value <- dhtLocalGet d h
      case value of
        Nothing -> fail "no value in local table for hash"
        Just value' -> putR d ns h value'

  dhtHashOutputBytes dht = div (dhtHashOutputBits dht) 8

  dhtHashMax d = 2 ^ dhtHashOutputBits d
  dhtHashI d h = 
    let hfold = 
          foldl (\i w -> (i `shiftL` 8) .|. toInteger w) (0 :: Integer)
        enc   = BS.unpack $ Ser.encode h
    in hfold $ take (dhtHashOutputBytes d) enc
       
  dhtDist d h1 h2 = 
    let h1i = dhtHashI d h1
        h2i = dhtHashI d h2
        hmax = dhtHashMax d
        d1 = abs $ h1i - h2i
        d2 = hmax - d1
    in min d1 d2

  dhtPut d bs =
    let h = dhtHash bs
        hdist = dhtDist d h 
        d0 = hdist (dhtNodeId d)
        closerP n = hdist n < d0 
    in do 
      dhtLocalInsert d h bs 
      hs <- dhtNodes d
      let closerNodes = 
            map fst $ sortBy (\(_, d1) (_, d2) -> compare d1 d2) 
            [(n, hdist n) | n <- filter closerP hs]
      case closerNodes of
        [] -> return $ Just h
        cns -> do
          res <- dhtTryPutRemotes d cns h 
          case res of 
            Nothing -> return $ Just h
            Just h' -> if (not $ h == h') 
                       then fail $ "unexpected hash from remote"
                       else do
                         dhtLocalDelete d h'
                         return $ Just h'
