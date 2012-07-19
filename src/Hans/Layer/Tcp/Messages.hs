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
