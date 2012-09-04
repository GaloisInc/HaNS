module Hans.Layer.Tcp.Messages where

import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window
import Hans.Message.Tcp
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F


-- Generic Packets -------------------------------------------------------------

mkSegment :: TcpSocket -> TcpHeader
mkSegment tcp = case tcpTimestamp tcp of
  Just ts -> setTcpOption (mkTimestamp ts) hdr
  Nothing -> hdr
  where
  hdr  = emptyTcpHeader
    { tcpDestPort   = sidRemotePort (tcpSocketId tcp)
    , tcpSourcePort = sidLocalPort (tcpSocketId tcp)
    , tcpSeqNum     = tcpSndNxt tcp
    , tcpAckNum     = tcpRcvNxt tcp
      -- XXX this doesn't really reflect the right number
    , tcpWindow     = fromIntegral (availableBytes (tcpInBuffer tcp))
    }

mkAck :: TcpSocket -> TcpHeader
mkAck tcp = addSackOption tcp
          $ (mkSegment tcp)
            { tcpAck = True
            }

-- | Add the Sack option to a tcp packet.
addSackOption :: TcpSocket -> TcpHeader -> TcpHeader
addSackOption sock
  | tcpSack sock && not (null bs) = setTcpOption (OptSack bs)
  | otherwise                     = id
  where
  bs = F.toList (localWindowSackBlocks (tcpIn sock))

-- | Add the sack permitted tcp option.
addSackPermitted :: TcpSocket -> TcpHeader -> TcpHeader
addSackPermitted sock
  | tcpSack sock = setTcpOption OptSackPermitted
  | otherwise    = id


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

mkSyn :: TcpSocket -> TcpHeader
mkSyn tcp = addSackPermitted tcp
          $ setTcpOption (mkMSS tcp)
          $ (mkSegment tcp)
            { tcpSyn    = True
            , tcpAckNum = 0
            }

-- | Construct a SYN ACK packet, in response to a SYN.
mkSynAck :: TcpSocket -> TcpHeader
mkSynAck tcp = addSackPermitted tcp
             $ setTcpOption (mkMSS tcp)
             $ (mkSegment tcp)
               { tcpSyn = True
               , tcpAck = True
               }


-- Connection Closing ----------------------------------------------------------

-- | Construct a FIN packet.
--
-- XXX should this include a sack option?
mkFinAck :: TcpSocket -> TcpHeader
mkFinAck tcp = (mkSegment tcp)
  { tcpFin = True
  , tcpAck = True
  }


-- Data Packets ----------------------------------------------------------------

mkData :: TcpSocket -> TcpHeader
mkData tcp = addSackOption tcp
           $ (mkSegment tcp)
             { tcpAck = True
             , tcpPsh = True
             }


-- Socket Actions --------------------------------------------------------------

syn :: Sock ()
syn  = do
  tcp <- getTcpSocket
  tcpOutput (mkSyn tcp) L.empty
  advanceSndNxt 1

-- | Respond to a SYN message with a SYN ACK message.
synAck :: Sock ()
synAck  = do
  advanceRcvNxt 1
  tcp <- getTcpSocket
  tcpOutput (mkSynAck tcp) L.empty
  advanceSndNxt 1

-- | Send an ACK packet.
ack :: Sock ()
ack  = do
  clearDelayedAck
  tcp <- getTcpSocket
  tcpOutput (mkAck tcp) L.empty

-- | Schedule a delayed ACK packet.
delayedAck :: Sock ()
delayedAck  = modifyTcpTimers_ (\tt -> tt { ttDelayedAck = True })

-- | Unschedule a delayed ACK packet.
clearDelayedAck :: Sock ()
clearDelayedAck  = modifyTcpTimers_ (\tt -> tt { ttDelayedAck = False })

-- | Send a FIN packet to begin closing a connection.
finAck :: Sock ()
finAck  = do
  tcp <- getTcpSocket
  tcpOutput (mkFinAck tcp) L.empty
  advanceSndNxt 1
  clearDelayedAck

-- | Send a segment.
outputSegment :: OutSegment -> Sock ()
outputSegment seg = do
  clearDelayedAck
  tcpOutput (outHeader seg) (outBody seg)


-- Flag Tests ------------------------------------------------------------------

isSyn :: TcpHeader -> Bool
isSyn hdr = foldr step (tcpSyn hdr) fields
  where
  step p r = r && not (p hdr)
  fields   = [ tcpCwr, tcpEce, tcpUrg, tcpAck, tcpPsh, tcpRst, tcpFin ]

isSynAck :: TcpHeader -> Bool
isSynAck hdr = foldr step (tcpSyn hdr && tcpAck hdr) fields
  where
  step p r = r && not (p hdr)
  fields   = [ tcpCwr, tcpEce, tcpUrg, tcpPsh, tcpRst, tcpFin ]

isRstAck :: TcpHeader -> Bool
isRstAck hdr = foldr step (tcpRst hdr && tcpAck hdr) fields
  where
  step p r = r && not (p hdr)
  fields   = [ tcpCwr, tcpEce, tcpUrg, tcpPsh, tcpSyn, tcpFin ]

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
