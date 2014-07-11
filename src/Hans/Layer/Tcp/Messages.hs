module Hans.Layer.Tcp.Messages where

import Hans.Layer ( time )
import Hans.Layer.Tcp.Monad
import Hans.Layer.Tcp.Types
import Hans.Layer.Tcp.Window
import Hans.Message.Tcp

import Data.Maybe ( fromMaybe )
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq


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
    , tcpWindow     = lwRcvWind (tcpIn tcp)
    }

mkAck :: TcpSocket -> TcpHeader
mkAck tcp = addSackOption tcp
          $ (mkSegment tcp)
            { tcpAck = True
            }


-- Options ---------------------------------------------------------------------

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

-- | Add the window scale option on the outgoing header.
addWindowScale :: TcpSocket -> TcpHeader -> TcpHeader
addWindowScale sock
  | tcpWindowScale sock = setTcpOption
                        $ OptWindowScaling
                        $ fromIntegral
                        $ lwRcvWindScale
                        $ tcpIn sock
  | otherwise           = id


-- Connection Refusal ----------------------------------------------------------

-- | Given a tcp header, generate the next header in the sequence that
-- corresponds to the RST ACK response.  As this should only be used in
-- situations in which an ACK was not received, this adds one plus the body
-- length to the ack number.
mkRstAck :: TcpHeader -> Int -> TcpHeader
mkRstAck hdr len = emptyTcpHeader
  { tcpSeqNum     = 0
  , tcpAckNum     = tcpSeqNum hdr + fromIntegral len + 1 -- to ack their message
  , tcpRst        = True
  , tcpAck        = True
  , tcpDestPort   = tcpSourcePort hdr
  , tcpSourcePort = tcpDestPort hdr
  }

mkRst :: TcpHeader -> TcpHeader
mkRst hdr = emptyTcpHeader
  { tcpSeqNum     = tcpAckNum hdr
  , tcpRst        = True
  , tcpDestPort   = tcpSourcePort hdr
  , tcpSourcePort = tcpDestPort hdr
  }


-- Connection Establishment ----------------------------------------------------

mkSyn :: TcpSocket -> TcpHeader
mkSyn tcp = addSackPermitted tcp
          $ addWindowScale tcp
          $ setTcpOption (mkMSS tcp)
          $ (mkSegment tcp)
            { tcpSyn    = True
            , tcpAckNum = 0
            }

-- | Construct a SYN ACK packet, in response to a SYN.
mkSynAck :: TcpSocket -> TcpHeader
mkSynAck tcp = addSackPermitted tcp
             $ addWindowScale tcp
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

-- | Queue an outgoing fin packet in the outgoing window, and send it.
--
-- NOTE:  This uses genSegments, which will pull data out of the waiti
finAck :: Sock ()
finAck  = do
  now <- inTcp time

  seg <- modifyTcpSocket $ \ tcp ->

    let -- construct the outgoing FIN,ACK segment
        hdr          = mkFinAck tcp
        finAckOutSeg = mkOutSegment now (ttRTO (tcpTimers tcp)) hdr L.empty

        -- push the FIN,ACK into the outgoing window
        tcp' = tcp { tcpOut    = addSegment finAckOutSeg (tcpOut tcp)

                     -- advance SND.NXT over the FIN
                   , tcpSndNxt = tcpSndNxt tcp + 1
                   }

     in (finAckOutSeg, tcp')

  outputSegment seg
  clearDelayedAck

rstAck :: TcpHeader -> Int -> Sock ()
rstAck hdr len = tcpOutput (mkRstAck hdr len) L.empty

rst :: TcpHeader -> Sock ()
rst hdr = tcpOutput (mkRst hdr) L.empty

-- | Send a segment.
outputSegment :: OutSegment -> Sock ()
outputSegment seg = do
  clearDelayedAck
  tcpOutput (outHeader seg) (outBody seg)


-- Flag Tests ------------------------------------------------------------------

type SetFlag   = TcpHeader -> Bool
type UnsetFlag = TcpHeader -> Bool

testFlags :: [SetFlag] -> [UnsetFlag] -> TcpHeader -> Bool
testFlags sfs ufs hdr = all test sfs && all (not . test) ufs
  where
  test prj = prj hdr

isSyn :: TcpHeader -> Bool
isSyn  = testFlags [ tcpSyn ]
                   [ tcpCwr, tcpEce, tcpUrg, tcpAck, tcpPsh, tcpRst, tcpFin ]

isSynAck :: TcpHeader -> Bool
isSynAck  = testFlags [ tcpSyn, tcpAck ]
                      [ tcpCwr, tcpEce, tcpUrg, tcpPsh, tcpRst, tcpFin ]

isRstAck :: TcpHeader -> Bool
isRstAck = testFlags [ tcpRst, tcpAck ]
                     [ tcpCwr, tcpEce, tcpUrg, tcpPsh, tcpSyn, tcpFin ]

isAck :: TcpHeader -> Bool
isAck = testFlags [ tcpAck ]
                  [ tcpCwr, tcpEce, tcpUrg, tcpPsh, tcpRst, tcpSyn, tcpFin ]

isFin :: TcpHeader -> Bool
isFin  = testFlags [ tcpFin ]
                   [ tcpCwr, tcpEce, tcpUrg, tcpAck, tcpPsh, tcpRst, tcpSyn ]

isFinAck :: TcpHeader -> Bool
isFinAck  = testFlags [ tcpFin, tcpAck ]
                      [ tcpCwr, tcpEce, tcpUrg, tcpPsh, tcpRst, tcpSyn ]


-- Packet Output ---------------------------------------------------------------

-- | Take data from the output buffer, and turn it into segments.  When data was
-- freed from the output buffer, the wakeup actions for any threads currently
-- blocked on writing to the output buffer will be returned.
genSegments :: POSIXTime -> TcpSocket -> (([Wakeup],OutSegments),TcpSocket)
genSegments now tcp0 = loop [] Seq.empty tcp0
  where
  loop ws segs tcp
    | rwAvailable (tcpOut tcp) <= 0 = result
    | otherwise                     = fromMaybe result $ do
      let len = nextSegSize tcp
      (mbWakeup,body,bufOut) <- takeBytes len (tcpOutBuffer tcp)
      let seg  = mkOutSegment now (ttRTO (tcpTimers tcp)) (mkData tcp) body
          tcp' = tcp { tcpSndNxt    = outAckNum seg
                     , tcpOut       = addSegment seg (tcpOut tcp)
                     , tcpOutBuffer = bufOut
                     }

      return (loop (addWakeup mbWakeup) (segs Seq.|> seg) tcp')
    where

    addWakeup Nothing  =   ws
    addWakeup (Just w) = w:ws

    result = ((ws,segs),tcp)
