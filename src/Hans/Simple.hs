module Hans.Simple (
    -- * UDP Messages
    renderUdp
  , renderIp4

    -- * Ident
  , Ident
  , newIdent
  , nextIdent
  ) where

import Hans.Address.IP4 (IP4)
import Hans.Message.Ip4 (IP4Packet(..),IP4Header(..),IP4Protocol(..)
                        ,mkIP4PseudoHeader,splitPacket,renderIP4Packet
                        ,emptyIP4Header)
import Hans.Message.Udp (UdpHeader(..),UdpPort,UdpPacket(..),renderUdpPacket)
import qualified Hans.Message.Ip4 as IP4

import Control.Concurrent (MVar,newMVar,modifyMVar)
import Data.Word (Word16)
import qualified Data.ByteString as S

newtype Ident = Ident (MVar IP4.Ident)

newIdent :: IO Ident
newIdent  = Ident `fmap` newMVar 0

nextIdent :: Ident -> IO IP4.Ident
nextIdent (Ident var) = modifyMVar var (\i -> return (i+1, i))

type MTU = Word16

fromMTU :: Maybe MTU -> Int
fromMTU  = maybe ip4Max (min ip4Max . fromIntegral)
  where ip4Max = 0xffff

-- | Render a UDP message to an unfragmented IP4 packet.
renderUdp :: Ident -> Maybe MTU -> IP4 -> IP4 -> UdpPort -> UdpPort
          -> S.ByteString
          -> IO [S.ByteString]
renderUdp i mb source dest srcPort destPort payload = do
  let prot = IP4Protocol 0x11
  let mk   = mkIP4PseudoHeader source dest prot
  let hdr  = UdpHeader
        { udpSourcePort = srcPort
        , udpDestPort   = destPort
        , udpChecksum   = 0
        }
  udp <- renderUdpPacket (UdpPacket hdr payload) mk
  renderIp4 i mb prot source dest udp


-- | Render an IP4 packet.
renderIp4 :: Ident -> Maybe MTU -> IP4Protocol -> IP4 -> IP4 -> S.ByteString
          -> IO [S.ByteString]
renderIp4 ident mb prot source dest payload = do
  i <- nextIdent ident
  let hdr = (emptyIP4Header prot source dest) { ip4Ident = i }
  mapM renderIP4Packet (splitPacket (fromMTU mb) (IP4Packet hdr payload))
