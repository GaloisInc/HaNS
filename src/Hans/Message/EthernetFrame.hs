{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Message.EthernetFrame (
    EtherType(..)
  , EthernetFrame(..)
  ) where

import Hans.Address.Mac (Mac)

import Data.Serialize (Serialize(..))
import Data.Serialize.Get (Get,getBytes,remaining)
import Data.Serialize.Put (Put,putByteString)
import Data.ByteString (ByteString)
import Data.Word (Word16)
import Numeric (showHex)

newtype EtherType = EtherType { getEtherType :: Word16 }
  deriving (Eq,Num,Ord,Serialize)

instance Show EtherType where
  showsPrec _ (EtherType et) = showString "EtherType 0x" . showHex et

data EthernetFrame = EthernetFrame
  { etherDest   :: !Mac
  , etherSource :: !Mac
  , etherType   :: !EtherType
  , etherData   :: ByteString
  } deriving (Eq,Show)

instance Serialize EthernetFrame where
  get = parseEthernetFrame
  put = renderEthernetFrame

parseEthernetFrame :: Get EthernetFrame
parseEthernetFrame  = do
  dst  <- get
  src  <- get
  ty   <- get
  body <- getBytes =<< remaining
  return $! EthernetFrame dst src ty body

renderEthernetFrame :: EthernetFrame -> Put
renderEthernetFrame (EthernetFrame s d t da) =
  put s >> put d >> put t >> putByteString da
