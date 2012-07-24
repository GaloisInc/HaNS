module Hans.Layer.Tcp.Messages where

import Hans.Layer.Tcp.Types
import Hans.Message.Tcp


-- Generic Packets -------------------------------------------------------------

mkSegment :: TcpSocket -> TcpHeader
mkSegment tcp = emptyTcpHeader
  { tcpDestPort   = sidRemotePort (tcpSocketId tcp)
  , tcpSourcePort = sidLocalPort (tcpSocketId tcp)
  , tcpSeqNum     = tcpSockSeq tcp
  , tcpAckNum     = tcpSockAck tcp
  , tcpWindow     = tcpSockWin tcp
  }

mkAck :: TcpSocket -> TcpHeader
mkAck tcp = (mkSegment tcp)
  { tcpAck = True
  }


-- Connection Refusal ----------------------------------------------------------

-- | Given a tcp header, generate the next header in the sequence that
-- corresponds to the RST ACK response.
mkRstAck :: TcpHeader -> TcpHeader
mkRstAck hdr = emptyTcpHeader
  -- XXX need a story for what to set the SN to here
  { tcpSeqNum     = 0
  , tcpAckNum     = tcpSeqNum hdr + 1
  , tcpSourcePort = tcpDestPort hdr
  , tcpDestPort   = tcpSourcePort hdr
  , tcpAck        = True
  , tcpRst        = True
  }


-- Connection Establishment ----------------------------------------------------

-- | Construct a SYN ACK packet, in response to a SYN.
mkSynAck :: TcpSocket -> TcpHeader
mkSynAck tcp = hdr
  { tcpAck    = True
  , tcpSyn    = True
  , tcpAckNum = tcpAckNum hdr
  , tcpSeqNum = tcpSeqNum hdr
  }
  where
  hdr = mkSegment tcp


-- Connection Closing ----------------------------------------------------------

-- | Construct a FIN packet.
mkFinAck :: TcpSocket -> TcpHeader
mkFinAck tcp = (mkSegment tcp)
  { tcpFin = True
  , tcpAck = True
  }


-- Flag Tests ------------------------------------------------------------------

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

isFinAck :: TcpHeader -> Bool
isFinAck hdr = foldr step (tcpFin hdr && tcpAck hdr) fields
  where
  step p r = r && not (p hdr)
  fields   = [ tcpCwr, tcpEce, tcpUrg, tcpPsh, tcpRst, tcpSyn ]
