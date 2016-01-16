{-# LANGUAGE RecordWildCards #-}

module Hans.Tcp.Message where

import Hans.Lens
import Hans.Tcp.Packet
import Hans.Tcp.Tcb

import qualified Data.ByteString as S
import           Data.IORef (readIORef)


-- | @<SEQ=SEG.ACK><CTL=RST>@
mkRst :: TcpHeader -> IO TcpHeader
mkRst TcpHeader { .. } =
     return $ set tcpRst True
            $ emptyTcpHeader { tcpSourcePort = tcpDestPort
                             , tcpDestPort   = tcpSourcePort
                             , tcpSeqNum     = tcpAckNum
                             }
{-# INLINE mkRst #-}


-- | @<SEQ=0><ACK=SEG.SEQ+SEG.LEN><CTL=RST,ACK>@
mkRstAck :: TcpHeader -> S.ByteString -> IO TcpHeader
mkRstAck hdr @ TcpHeader { .. } payload =
     return $ set tcpRst True
            $ set tcpAck True
            $ emptyTcpHeader { tcpSourcePort = tcpDestPort
                             , tcpDestPort   = tcpSourcePort
                             , tcpSeqNum     = 0
                             , tcpAckNum     = tcpSegNextAckNum hdr (S.length payload)
                             }
{-# INLINE mkRstAck #-}


mkSyn :: Tcb -> IO TcpHeader
mkSyn Tcb { .. } =
  do iss <- readIORef tcbIss
     return $ set tcpSyn True
            $ emptyTcpHeader { tcpSourcePort = tcbRemotePort
                             , tcpDestPort   = tcbLocalPort
                             , tcpSeqNum     = iss
                             , tcpAckNum     = 0
                             }
{-# INLINE mkSyn #-}


-- | @<SEQ=ISS><ACK=RCV.NXT><CTL=SYN,ACK>@
mkSynAck :: Tcb -> TcpHeader -> IO TcpHeader
mkSynAck Tcb { .. } TcpHeader { .. } =
  do iss <- readIORef tcbIss
     ack <- getRcvNxt tcbRecvWindow

     return $ set tcpSyn True
            $ set tcpAck True
            $ emptyTcpHeader { tcpSourcePort = tcpDestPort
                             , tcpDestPort   = tcpSourcePort
                             , tcpSeqNum     = iss
                             , tcpAckNum     = ack
                             }
{-# INLINE mkSynAck #-}


mkAck :: TcpSeqNum    -- ^ SEG.SEQ
      -> TcpSeqNum    -- ^ SEG.ACK
      -> TcpPort      -- ^ Local port
      -> TcpPort      -- ^ Remote port
      -> IO TcpHeader
mkAck segSeq segAck local remote =
     return $ set tcpAck True
            $ emptyTcpHeader { tcpSourcePort = local
                             , tcpDestPort   = remote
                             , tcpSeqNum     = segSeq
                             , tcpAckNum     = segAck
                             }
{-# INLINE mkAck #-}
