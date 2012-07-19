module Hans.Layer.Tcp.Messages where

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

mkSynAck :: TcpHeader -> TcpHeader
mkSynAck hdr = emptyTcpHeader
  { tcpSeqNum     = tcpSeqNum hdr
  , tcpAckNum     = TcpAckNum (getSeqNum (tcpSeqNum hdr) + 1)
  , tcpSourcePort = tcpDestPort hdr
  , tcpDestPort   = tcpSourcePort hdr
  , tcpAck        = True
  , tcpSyn        = True
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
