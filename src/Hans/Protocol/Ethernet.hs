{-# LANGUAGE RecordWildCards #-}

module Hans.Protocol.Ethernet (
    EthernetFrame(..),
    parseEthernetFrame,
    renderEthernetFrame
  ) where

import Hans.Address.Mac (Mac,parseMac,renderMac)
import Hans.Utils (chunk)

import Data.Serialize.Get (Get,remaining,getWord16be,getBytes,runGet)
import Data.Serialize.Put (Putter,putWord16be,runPut)


-- Mac Addresses ---------------------------------------------------------------

data Mac = Mac { macBytes :: !S.ByteString
               } deriving (Show)

parseMac :: Get Mac
parseMac  =
  do macBytes <- getBytes 6
     return Mac { .. }

putMac :: 


-- Ethernet Frames -------------------------------------------------------------

data EthernetFrame = EthernetFrame
  { eDest    :: !Mac
  , eSource  :: !Mac
  , eType    :: !Word16
  } deriving (Eq,Show)

parseEthernetFrame :: S.ByteString -> Get (EthernetFrame,S.ByteString)
parseEthernetFrame =
  do eDest   <- parseMac
     eSource <- parseMac
     eType   <- getWord16be
     payload <- getBytes =<< remaining
     return (EthernetFrame { .. }, payload)

renderEthernetFrame :: Putter EthernetFrame
renderEthernetFrame EthernetFrame { .. } =
  do renderMac       eDest
     renderMac       eSource
     renderEtherType eType
