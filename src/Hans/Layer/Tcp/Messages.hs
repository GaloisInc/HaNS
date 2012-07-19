module Hans.Layer.Tcp.Messages where

import Hans.Message.Tcp


-- | Given a tcp header, generate the next header in the sequence that
-- corresponds to the RST ACK response.
mkRstAck :: TcpHeader -> TcpHeader
mkRstAck hdr = swapPorts $ resetFlags hdr
  { tcpSeqNum = TcpSeqNum 0
  , tcpAckNum = TcpAckNum (getSeqNum (tcpSeqNum hdr) + 1)
  , tcpAck    = True
  , tcpRst    = True
  }



swapPorts :: TcpHeader -> TcpHeader
swapPorts hdr = hdr
  { tcpSourcePort = tcpDestPort hdr
  , tcpDestPort   = tcpSourcePort hdr
  }

resetFlags :: TcpHeader -> TcpHeader
resetFlags hdr = hdr
  { tcpCwr           = False
  , tcpEce           = False
  , tcpUrg           = False
  , tcpAck           = False
  , tcpPsh           = False
  , tcpRst           = False
  , tcpSyn           = False
  , tcpFin           = False
  }
