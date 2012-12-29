{-# LANGUAGE NoMonomorphismRestriction #-}
module Crypto.OpenPGP.Conduit (keyGenSource) where

import Control.Monad.IO.Class
import System.FilePath
import System.Directory
import System.Process
import System.IO
import System.Posix.Temp
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Binary as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import qualified Data.Conduit.Util as UC 
import qualified Data.OpenPGP as OpenPGP
import qualified Data.OpenPGP.CryptoAPI as OpenPGP


messageList' :: (Monad m) => Bool -> OpenPGP.Message -> GSource m OpenPGP.Packet
messageList' False (OpenPGP.Message ps) = sourceList ps
messageList' True  msg = 
  let source = messageList' False msg
  in do
    
      (cs,ncs) = partition isCompressedPacket ps
      dcs = concat $  L.map (\(OpenPGP.CompressedDataPacket _ (OpenPGP.Message sps)) -> sps) cs
  in sourceList $ ncs ++ dcs

messageList :: Monad m => OpenPGP.Message -> GSource m OpenPGP.Packet
messageList = messageList' False


isOnePassSignaturePacket (OpenPGP.OnePassSignaturePacket _ _ _ _ _ _) = True
isOnePassSignaturePacket _ = False
isOnePassSignatureFilter = Data.Conduit.List.filter isOnePassSignaturePacket

isCompressedPacket (OpenPGP.CompressedDataPacket _ _) = True
isCompressedPacket _ = False
isCompressedPacketFilter = Data.Conduit.List.filter isCompressedPacket
      

keyGenSource :: MonadResource m => String -> String -> String -> 
                    Pipe l i o u m (OpenPGP.Message, OpenPGP.Message)
keyGenSource name comment mail = 
  let cmd = "gpg"
      args = ["--openpgp", "--batch", "--no-default-keyring", "--gen-key" ]
      alloc = do
        (gi',go,ge,gp) <- createProcess $ (proc cmd args) { std_in = CreatePipe }
        case (gi') of
          Nothing -> error "unable to open gpg stdin"
          Just gi -> do
            (pubkey,h1) <- mkstemp $ "/tmp" </> cmd  ++ "XXXXXX"
            (privkey,h2) <- mkstemp $ "/tmp" </> cmd ++ "XXXXXX"
            hClose h1
            hClose h2
            return (gi, gp, pubkey, privkey)
      fin (gi, gp, pubkey_fp , privkey_fp) = do
         terminateProcess gp
         _ <- waitForProcess gp
         removeFile pubkey_fp
         removeFile privkey_fp
         return ()
      pipeF (gi, gp, pubkey_fp, privkey_fp) = do
         let dk = genKeysSource name comment mail pubkey_fp privkey_fp
         let pubkey = mapOutput (B.decode . BL.fromChunks . (:[]) ) $ 
                        sourceIOHandle $ openFile pubkey_fp ReadMode 
         let privkey = mapOutput (B.decode . BL.fromChunks . (:[]) ) $ 
                         sourceIOHandle $ openFile privkey_fp ReadMode

         runPipe $ do
           runPipe $ yield dk $$ sinkHandle gi
           liftIO $ do hClose gi
                       waitForProcess gp
         mpks <- UC.zip pubkey privkey $$ await
         case mpks of 
           Nothing -> error "unable to read keyrings"
           Just pks -> return pks
  in bracketP alloc fin pipeF

genKeysSource :: String  -> String -> String -> String -> String -> BS.ByteString  
genKeysSource name comment mail pub priv = (BS.pack . UTF8.encode . unlines)
                ["Key-Type: RSA",
                 "Key-Usage: encrypt,sign,auth",
                 "Key-Length: 1024",
                 "Name-Real: " ++ name,
                 "Name-Comment: " ++ comment,
                 "Name-Email: " ++ mail,
                 "Expire-Date: 0",
                 "%pubring " ++ pub,
                 "%secring " ++ priv,
                 "%commit" ]
