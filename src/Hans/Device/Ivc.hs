{-# LANGUAGE MultiParamTypeClasses #-}

module Hans.Device.Ivc (
    ivcSend
  , ivcReceiveLoop
  , Bytes(getBytes)
  ) where

import Hans.Layer.Ethernet (EthernetHandle,queueEthernet)

import Communication.IVC as IVC (put,OutChannelEx,get,InChannelEx,Bin)
import Control.Monad (forever,when)
import Data.Serialize (Serialize(get,put),getByteString,putByteString,remaining)
import qualified Data.ByteString as S

newtype Bytes = Bytes
  { getBytes :: S.ByteString
  } deriving Show

instance Serialize Bytes where
  get = Bytes `fmap` (getByteString =<< remaining)
  put = putByteString . getBytes

ivcSend :: OutChannelEx Bin Bytes -> S.ByteString -> IO ()
ivcSend chan = IVC.put chan . Bytes

ivcReceiveLoop :: InChannelEx Bin Bytes -> EthernetHandle -> IO ()
ivcReceiveLoop chan eth = forever $ do
  Bytes bs <- IVC.get chan
  when (S.length bs > 14) (queueEthernet eth bs)
