{--
Copyright (c) 2006, Peng Li
              2006, Stephan A. Zdancewic
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

* Neither the name of the copyright owners nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--}

module Network.TCP.Type.Datagram 
( TCPSegment (..)
, UDPDatagram (..)
, Protocol (..)
, ICMPType (..)
, ICMPDatagram (..)
, IPMessage (..)
, tcp_ws
, set_tcp_ws
, tcp_mss
, set_tcp_mss
, tcp_ts
, set_tcp_ts
, tcp_seq
, tcp_ack
, tcp_URG
, tcp_ACK
, tcp_PSH
, tcp_RST
, tcp_SYN
, tcp_FIN
, tcp_win
, tcp_urp
, mkTCPSegment
, mkTCPSegment'
)
where

import Hans.Address.IP4 (IP4,convertToWord32)
import Hans.Message.Tcp
    (TcpHeader(..),TcpPacket(..),TcpPort(..),TcpAckNum(..),TcpSeqNum(..)
    ,findTcpOption,setTcpOption,TcpOptionTag(..),TcpOption(..))

import Network.TCP.Type.Base

data TCPSegment = TCPSegment
  { tcp_src    :: !TCPAddr
  , tcp_dst    :: !TCPAddr
  , tcp_header :: !TcpHeader
  , tcp_data   :: !BufferChain
  }

mkTCPSegment :: IP4 -> IP4 -> TcpPacket -> TCPSegment
mkTCPSegment src dst (TcpPacket hdr body) = TCPSegment
  { tcp_src    = TCPAddr (IPAddr (convertToWord32 src),srcP)
  , tcp_dst    = TCPAddr (IPAddr (convertToWord32 dst),dstP)
  , tcp_header = hdr
  , tcp_data   = bufferchain_singleton body
  }
  where
  TcpPort srcP = tcpSourcePort hdr
  TcpPort dstP = tcpDestPort hdr

mkTCPSegment' :: TCPAddr -> TCPAddr -> TcpHeader -> BufferChain -> TCPSegment
mkTCPSegment' s@(TCPAddr (_, srcP)) d@(TCPAddr (_, dstP)) hdr body =
  TCPSegment
    { tcp_src    = s
    , tcp_dst    = d
    , tcp_header = hdr
      { tcpSourcePort = TcpPort srcP
      , tcpDestPort   = TcpPort dstP
      }
    , tcp_data   = body
    }
  where

tcp_seq = getSeqNum . tcpSeqNum . tcp_header
tcp_ack = getAckNum . tcpAckNum . tcp_header
tcp_URG = tcpUrg    . tcp_header
tcp_ACK = tcpAck    . tcp_header
tcp_PSH = tcpPsh    . tcp_header
tcp_RST = tcpRst    . tcp_header
tcp_SYN = tcpSyn    . tcp_header
tcp_FIN = tcpFin    . tcp_header
tcp_win = tcpWindow . tcp_header
tcp_urp = tcpUrgentPointer . tcp_header

tcp_ws :: TCPSegment -> Maybe Int
tcp_ws = fmap prj . findTcpOption OptTagWindowScaling . tcp_header
  where
  prj (OptWindowScaling ws) = fromIntegral ws

set_tcp_ws :: Maybe Int -> TcpHeader -> TcpHeader
set_tcp_ws Nothing   = id
set_tcp_ws (Just ws) = setTcpOption (OptWindowScaling (fromIntegral ws))

tcp_mss :: TCPSegment -> Maybe Int
tcp_mss = fmap prj . findTcpOption OptTagMaxSegmentSize . tcp_header
  where
  prj (OptMaxSegmentSize mss) = fromIntegral mss

set_tcp_mss :: Maybe Int -> TcpHeader -> TcpHeader
set_tcp_mss Nothing    = id
set_tcp_mss (Just mss) = setTcpOption (OptMaxSegmentSize (fromIntegral mss))

tcp_ts :: TCPSegment -> Maybe (Timestamp,Timestamp)
tcp_ts = fmap prj . findTcpOption OptTagTimestamp . tcp_header
  where
  prj (OptTimestamp v r) = (Timestamp v, Timestamp r)

set_tcp_ts :: Maybe (Timestamp,Timestamp) -> TcpHeader -> TcpHeader
set_tcp_ts Nothing                           = id
set_tcp_ts (Just (Timestamp v, Timestamp r)) = setTcpOption (OptTimestamp v r)

{-
data TCPSegment = TCPSegment
   { tcp_src  :: !TCPAddr
   , tcp_dst  :: !TCPAddr
   , tcp_seq  :: !SeqLocal
   , tcp_ack  :: !SeqForeign
   , tcp_URG  :: !Bool
   , tcp_ACK  :: !Bool
   , tcp_PSH  :: !Bool
   , tcp_RST  :: !Bool
   , tcp_SYN  :: !Bool
   , tcp_FIN  :: !Bool
   , tcp_win  :: !Int
   , tcp_urp  :: !Int
   , tcp_data :: !BufferChain
   -- option: window scaling
   , tcp_ws   :: !(Maybe Int)
   -- option: max segment size
   , tcp_mss  :: !(Maybe Int)
   -- option: RFC1323
   , tcp_ts   :: !(Maybe (Timestamp, Timestamp))
   } -}

instance Show TCPSegment where
  show seg = 
   let part1 = 
          if (get_port $ tcp_src seg) > 9999 || (get_port $ tcp_dst seg) == 8888 then
            "<==" ++ (show $ tcp_src seg) 
            ++ " ack=" ++(show $ seq_val $ tcp_ack seg)
            ++ " seq=" ++(show $ seq_val $ tcp_seq seg)
          else
            "==>" ++ (show $ tcp_dst seg) 
            ++ " seq=" ++(show $ seq_val $ tcp_seq seg)
            ++ " ack=" ++(show $ seq_val $ tcp_ack seg)
   in
   let part2 =
        " ["++
        (if tcp_URG seg then " URG(urp="++ (show $ tcp_urp seg) ++ ")" else "") ++
        (if tcp_SYN seg then " SYN" else "") ++
        (if tcp_FIN seg then " FIN" else "") ++
        (if tcp_RST seg then " RST" else "") ++
        (if tcp_ACK seg then " ACK" else "") ++
        (if tcp_PSH seg then " PSH" else "") ++
        " ]"
     in
     part1 
      ++ " WIN=" ++(show $ tcp_win seg)
      ++ " LEN=" ++(show $ bufc_length $ tcp_data seg)
      ++ part2

data UDPDatagram = UDPDatagram
   { udp_src  :: TCPAddr
   , udp_dst  :: TCPAddr
   , udp_data :: [Char]
   } deriving (Show, Eq)

data Protocol = PROTO_TCP | PROTO_UDP deriving (Show, Eq)

data ICMPType = 
     ICMP_UNREACH Int
   | ICMP_SOURCE_QUENCE Int
   | ICMP_REDIRECT Int
   | ICMP_TIME_EXCEEDED Int
   | ICMP_PARAMPROB Int
   deriving (Show, Eq)

data ICMPDatagram = ICMPDatagram
  { icmp_send   :: IPAddr
  , icmp_recv   :: IPAddr
  , icmp_src    :: Maybe TCPAddr
  , icmp_dst    :: Maybe TCPAddr
  , icmp_proto  :: Protocol
  , icmp_seq    :: Maybe SeqLocal
  , icmp_t      :: ICMPType
  } deriving (Show, Eq)

data IPMessage = TCPMessage !TCPSegment
               | ICMPMessage !ICMPDatagram
               | UDPMessage !UDPDatagram
                 deriving (Show)
