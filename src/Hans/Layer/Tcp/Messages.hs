module Hans.Layer.Tcp.Messages where

import Hans.Layer.Tcp.Types
import Hans.Message.Tcp


-- | Given a tcp header, generate the next header in the sequence that
-- corresponds to the RST ACK response.
mkRstAck :: TcpHeader -> TcpHeader
mkRstAck hdr = emptyTcpHeader
  { tcpSeqNum     = TcpSeqNum 0
  , tcpAckNum     = TcpAckNum (getSeqNum (tcpSeqNum hdr) + 1)
  , tcpSourcePort = tcpDestPort hdr
  , tcpDestPort   = tcpSourcePort hdr
  , tcpAck        = True
  , tcpRst        = True
  }

-- | Construct a SYN ACK packet, in response to a SYN.
mkSynAck :: TcpSeqNum -> TcpHeader -> TcpHeader
mkSynAck sn hdr = emptyTcpHeader
  { tcpSeqNum     = sn
  , tcpAckNum     = TcpAckNum (getSeqNum (tcpSeqNum hdr) + 1)
  , tcpSourcePort = tcpDestPort hdr
  , tcpDestPort   = tcpSourcePort hdr
  , tcpAck        = True
  , tcpSyn        = True
  }

-- | Construct a FIN packet.
mkCloseFin :: TcpSocket -> TcpHeader
mkCloseFin tcp = emptyTcpHeader
  { tcpFin        = True
  , tcpDestPort   = sidRemotePort (tcpSocketId tcp)
  , tcpSourcePort = sidLocalPort (tcpSocketId tcp)
  }

isSyn :: TcpHeader -> Bool
isSyn hdr = foldr step (tcpSyn hdr) fields
  where
  step p r = r && not (p hdr)
  fields   = [ tcpCwr, tcpEce, tcpUrg, tcpAck, tcpPsh, tcpRst, tcpFin ]

isAck :: TcpHeader -> Bool
isAck hdr = foldr step (tcpAck hdr) fields
  where
  step p r = r && not (p hdr)
  fields   = [ tcpCwr, tcpEce, tcpUrg, tcpPsh, tcpRst, tcpSyn, tcpFin ]

isFin :: TcpHeader -> Bool
isFin hdr = foldr step (tcpFin hdr) fields
  where
  step p r = r && not (p hdr)
  fields   = [ tcpCwr, tcpEce, tcpUrg, tcpAck, tcpPsh, tcpRst, tcpSyn ]
