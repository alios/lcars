{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, DeriveDataTypeable #-}

module Lcars (LcarsNode(..), LcarsNodeT, lnName, lnDomain) where
import Data.Typeable (Typeable(..))
import Data.Data (Data(..))
import Data.Binary 
import Data.Binary.Get (getBytes)
import Control.Monad.IO.Class
import Data.Conduit
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Crypto.OpenPGP.Conduit
import Control.Distributed.Process.Closure
import qualified Data.OpenPGP as OpenPGP  
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString as BS

class (Serializable n) => LcarsNode n where
  type NodeState n ::  *
  
  defaultNode :: n
  nodeMain :: n -> Process ()

data LcarsNodeT = LcarsNode {
  lnName :: String,
  lnDomain :: String
  } deriving (Show, Eq, Data, Typeable)

instance Binary LcarsNodeT where
  put n = do
    put . length $ name
    put name
    put . length $ domain
    put domain
    where 
      name = UTF8.encode . lnName $ n
      domain = UTF8.encode . lnDomain $ n
  get = do
    ln <- get :: Get Int
    ns <- fmap (UTF8.decode . BS.unpack) $ getBytes ln
    ld <- get :: Get Int
    ds <- fmap (UTF8.decode . BS.unpack) $ getBytes ln
    return $ LcarsNode ns ds
  
  
instance LcarsNode LcarsNodeT where
  defaultNode = LcarsNode "unset" "node.local"
  nodeMain n = do 
    foo <- keyGen n
    return ()
  
fqdn :: LcarsNodeT -> String  
fqdn n = concat [lnName n, "@", lnDomain n]    

keyGen :: MonadIO m => LcarsNodeT -> m (OpenPGP.Message, OpenPGP.Message)
keyGen n = 
  let pipe = keyGenSource (lnName n) (fqdn n) "lcars network node"
  in liftIO . runResourceT . runPipe $ pipe
