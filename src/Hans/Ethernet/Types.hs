{-# LANGUAGE RecordWildCards #-}

module Hans.Ethernet.Types (
    EthernetHeader(..), getEthernetHeader, putEthernetHeader,
    Mac(..), getMac, putMac
  ) where

import           Data.Serialize.Get (Get,getWord16be,getBytes)
import           Data.Serialize.Put (Putter,putWord16be,putByteString)
import qualified Data.ByteString as S
import           Data.Word (Word16)


-- Mac Addresses ---------------------------------------------------------------

newtype Mac = Mac { macBytes :: S.ByteString
                  } deriving (Eq,Ord,Show)

getMac :: Get Mac
getMac  =
  do macBytes <- getBytes 6
     return Mac { .. }

putMac :: Putter Mac
putMac Mac { .. } = putByteString macBytes


-- Ethernet Frames -------------------------------------------------------------

data EthernetHeader = EthernetHeader
  { eDest    :: !Mac
  , eSource  :: !Mac
  , eType    :: {-# UNPACK #-} !Word16
  } deriving (Eq,Show)

getEthernetHeader :: Get EthernetHeader
getEthernetHeader =
  do eDest   <- getMac
     eSource <- getMac
     eType   <- getWord16be
     return EthernetHeader { .. }

putEthernetHeader :: Putter EthernetHeader
putEthernetHeader EthernetHeader { .. } =
  do putMac      eDest
     putMac      eSource
     putWord16be eType
