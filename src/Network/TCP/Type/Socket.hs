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

module Network.TCP.Type.Socket 
where

import Network.TCP.Type.Base
import Network.TCP.Type.Timer
import Network.TCP.Type.Datagram
import Network.TCP.Type.Syscall
import Data.Map as Map

data TCPState = CLOSED
              | LISTEN
              | SYN_SENT
              | SYN_RECEIVED
              | ESTABLISHED
              | CLOSE_WAIT
              | FIN_WAIT_1
              | FIN_WAIT_2
              | CLOSING
              | LAST_ACK
              | TIME_WAIT
                deriving (Show,Eq)

data TCPReassSegment = TCPReassSegment 
    { trs_seq         :: !SeqForeign
    , trs_FIN         :: !Bool
    , trs_data        :: !BufferChain
    } deriving (Show)

data RexmtMode = RexmtSyn
               | Rexmt
               | Persist
                 deriving (Show,Eq)

data Rttinf = Rttinf
    { t_rttupdated  :: !Int
    , tf_srtt_valid :: !Bool
    , t_srtt        :: !Time
    , t_rttvar      :: !Time
    , t_rttmin      :: !Time
    , t_lastrtt     :: !Time
    , t_lastshift   :: !Int
    , t_wassyn      :: !Bool
    } deriving (Show)


data IOBC = NO_OOBDATA | OOBDATA Buffer | HAD_OOBDATA deriving (Show)

data SocketListen = SocketListen
    { lis_q0     :: ![SocketID] -- q0 
    , lis_q      :: ![SocketID] -- q
    , lis_qlimit :: !Int
    } deriving (Show,Eq)

data TCBTiming = TCBTiming
    { tt_keep        :: !(Maybe Time)
    , tt_conn_est    :: !(Maybe Time)
    , tt_fin_wait_2  :: !(Maybe Time)
    , tt_2msl        :: !(Maybe Time)
    , t_idletime     :: !Time   
    , ts_recent      :: !(TimeWindow Timestamp)
    , t_badrxtwin    :: !(TimeWindow ()) 
    } deriving (Show)
data TCBSending = TCBSending
    { sndq            :: !BufferChain
    , snd_una         :: !SeqLocal
    , snd_wnd         :: !Int
    , snd_wl1         :: !SeqForeign
    , snd_wl2         :: !SeqLocal
    , snd_cwnd        :: !Int
    , snd_nxt         :: !SeqLocal
    , snd_max         :: !SeqLocal
    , t_dupacks       :: !Int
    , t_rttinf        :: !Rttinf
    , t_rttseg        :: !(Maybe (Timestamp, SeqLocal))
    , tt_rexmt        :: !(Maybe (Timed (RexmtMode, Int)))
    } deriving (Show)
data TCBReceiving = TCBReceiving
    { last_ack_sent   :: !SeqForeign
    , tf_rxwin0sent   :: !Bool
    , tf_shouldacknow :: !Bool
    , tt_delack       :: !Bool
    , rcv_adv         :: !SeqForeign
    , rcv_wnd         :: !Int
    , rcv_nxt         :: !SeqForeign
    , rcvq            :: !BufferChain
    , t_segq          :: ![TCPReassSegment]
    } deriving (Show)
data TCBMisc = TCBMisc
    { -- retransmission
      snd_ssthresh      :: !Int
    , snd_cwnd_prev     :: !Int
    , snd_ssthresh_prev :: !Int
    , snd_recover       :: !SeqLocal
    -- some tags
    , cantsndmore       :: !Bool
    , cantrcvmore       :: !Bool
    , bsd_cantconnect   :: !Bool -- not very useful...
    -- initialization parameters
    , self_id           :: !SocketID
    , parent_id         :: !SocketID
    , local_addr        :: !TCPAddr
    , remote_addr       :: !TCPAddr
    , t_maxseg          :: !Int
    , t_advmss          :: !(Maybe Int)
    , tf_doing_ws       :: !Bool       
    , tf_doing_tstmp    :: !Bool
    , tf_req_tstmp      :: !Bool
    , request_r_scale   :: !(Maybe Int)
    , snd_scale         :: !Int
    , rcv_scale         :: !Int
    , iss               :: !SeqLocal
    , irs               :: !SeqForeign
    -- other things i don't use for the moment
    , sndurp            :: !(Maybe Int)
    , rcvurp            :: !(Maybe Int)
    , iobc              :: !IOBC
    , rcv_up            :: !SeqForeign
    , tf_needfin        :: !Bool
    } deriving (Show)

data TCPSocket threadt = TCPSocket
    { st      :: !TCPState
    , cb_time :: !TCBTiming
    , cb_snd  :: !TCBSending
    , cb_rcv  :: !TCBReceiving
    , cb      :: !TCBMisc
    , sock_listen  :: !SocketListen
    -- suspended commands (threads)
    , waiting_list :: ![(SockReq, SockRsp -> threadt)]
    } 

instance Show (TCPSocket t) where
  show (TCPSocket s cb1 cb2 cb3 cb4 lis wl) = 
     "TCPSocket state ="++(show s) ++ "\n" ++
       "   " ++ (show cb1) ++ "\n" ++ 
       "   " ++ (show cb2) ++ "\n" ++ 
       "   " ++ (show cb3) ++ "\n" ++ 
       "   " ++ (show cb4) ++ "\n" ++ 
       "   " ++ (show lis) ++ "\n" ++
       "   waiting: " ++ (show $ length wl)

data Host threadt = Host
    { sock_map        :: !(Map SocketID (TCPSocket threadt))
    , output_queue    :: ![IPMessage]
    , ready_list      :: ![threadt]
    , ticks           :: !Timestamp
    , clock           :: !Time
    , next_timers     :: !(Time,Time) -- fast timer, slow timer
    , local_ports     :: ![Port]
    }

empty_host :: Host t
empty_host = Host
  { sock_map        = Map.empty
  , output_queue    = []
  , ready_list      = []
  , ticks           = Timestamp 0
  , clock           = 0
  , next_timers     = (0,0)
  , local_ports     = [0..65535]
  }

update_host_time :: Time -> Host t -> Host t
update_host_time now h = h
  { clock = now
  }
