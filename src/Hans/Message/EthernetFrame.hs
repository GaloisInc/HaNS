{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Message.EthernetFrame (
    EtherType(..)
  , parseEtherType, renderEtherType

  , EthernetFrame(..)
  , parseEthernetFrame, renderEthernetFrame
  ) where

import Hans.Address.Mac (Mac,parseMac,renderMac)
import Hans.Utils (chunk)

import Control.Applicative ((<$>),(<*>))
import Data.Serialize.Get (Get,remaining,getWord16be,getBytes,runGet)
import Data.Serialize.Put (Putter,putWord16be,runPut)
import Data.Word (Word16)
import Numeric (showHex)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S


-- Ether Type ------------------------------------------------------------------

newtype EtherType = EtherType { getEtherType :: Word16 }
  deriving (Eq,Num,Ord)

instance Show EtherType where
  showsPrec _ (EtherType et) = showString "EtherType 0x" . showHex et

parseEtherType :: Get EtherType
parseEtherType  = EtherType <$> getWord16be

renderEtherType :: Putter EtherType
renderEtherType  = putWord16be . getEtherType


-- Ethernet Frames -------------------------------------------------------------

data EthernetFrame = EthernetFrame
  { etherDest   :: !Mac
  , etherSource :: !Mac
  , etherType   :: !EtherType
  } deriving (Eq,Show)

parseEthernetFrame :: S.ByteString -> Either String (EthernetFrame,S.ByteString)
parseEthernetFrame = runGet ((,) <$> header <*> (getBytes =<< remaining))
  where
  header =  EthernetFrame
        <$> parseMac
        <*> parseMac
        <*> parseEtherType

renderEthernetFrame :: EthernetFrame -> L.ByteString -> L.ByteString
renderEthernetFrame frame body = chunk hdr `L.append` body
  where
  hdr = runPut $ do
    renderMac         (etherDest   frame)
    renderMac         (etherSource frame)
    renderEtherType   (etherType   frame)
