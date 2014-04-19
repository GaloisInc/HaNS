{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Hans.Device.Ivc (
    ivcSend
  , ivcReceiveLoop
  ) where

import Hans.Layer.Ethernet (EthernetHandle,queueEthernet)
import Communication.IVC as IVC
import Control.Monad (forever,when)
import qualified Data.ByteString as S

ivcSend :: IVC.WriteableChan c S.ByteString => c -> S.ByteString -> IO ()
ivcSend chan = IVC.put chan

ivcReceiveLoop :: IVC.ReadableChan c S.ByteString => c -> EthernetHandle -> IO ()
ivcReceiveLoop chan eth = forever $
  do bs <- IVC.get chan
     when (S.length bs > 14) (queueEthernet eth bs)
