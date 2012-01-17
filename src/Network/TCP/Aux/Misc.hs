{-# LANGUAGE ScopedTypeVariables #-}
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

module Network.TCP.Aux.Misc where

import Network.TCP.Aux.Param
import Network.TCP.Type.Base
import Network.TCP.Type.Socket
import Network.TCP.Type.Timer

import Control.Exception
import Data.List as List
import Data.Map as Map
import Foreign


bound_ports :: Map SocketID (TCPSocket threadt) -> [Port]
bound_ports sockmap = List.map get_local_port (keys sockmap)

-- not considering SO_REUSEADDR
-- bound_port_allowed :: Map SocketID (TCPSocket threadt) -> Port -> Bool
-- bound_port_allowed m p = not $ List.elem p (bound_ports m)

-- lookup_socketid_by_seg :: Map SocketID (TCPSocket threadt) -> TCPSegment -> Maybe SocketID
-- lookup_socketid_by_seg m s =
--     let fakeid = (tcp_dst s, tcp_src s) in
--         if (member fakeid m) then
--            Just fakeid
--          else
--            Nothing
--
create_timer :: Time -> Time -> Time
create_timer curr_time offset = curr_time + offset

slow_timer :: Time -> Time -> Time
slow_timer  = create_timer

create_timewindow :: Time -> Time -> a -> Maybe (Timed a)
create_timewindow curr_time offset a =
  Just (Timed a (create_timer curr_time offset))

-- queues

-- enqueue_message msg q = addToQueue q msg
-- enqueue_messages msgs q = foldl addToQueue q msgs

accept_incoming_q0 :: SocketListen -> Bool
accept_incoming_q0 lis = length (lis_q lis) < backlog_fudge (lis_qlimit lis)

accept_incoming_q :: SocketListen -> Bool
accept_incoming_q lis =
    length (lis_q lis) < 3 * (backlog_fudge (lis_qlimit lis `div` 2))

drop_from_q0 :: SocketListen -> Bool
drop_from_q0 lis = length (lis_q0 lis) >= tcp_q0maxlimit

do_tcp_options :: Time -> Bool -> (TimeWindow Timestamp) -> Timestamp -> Maybe (Timestamp,Timestamp)
do_tcp_options curr_time cb_tf_doing_tstmp cb_ts_recent cb_ts_val =
    if cb_tf_doing_tstmp then
       let ts_ecr' = case timewindow_val curr_time cb_ts_recent of
            Just x -> x
            Nothing -> Timestamp 0
        in Just(cb_ts_val, ts_ecr')
     else
       Nothing

calculate_tcp_options_len :: Num a => Bool -> a
calculate_tcp_options_len cb_tf_doing_tstmp
  | cb_tf_doing_tstmp = 12
  | otherwise         = 0

rounddown :: Integral a => a -> a -> a
rounddown bs v
  | v < bs    = v
  | otherwise = (v `div` bs) * bs

roundup :: Integral a => a -> a -> a
roundup bs v = ((v+(bs-1)) `div` bs) * bs

calculate_buf_sizes :: Int -> Maybe Int -> Maybe Int -> Bool -> Int -> Int
                    -> Bool -> (Int,Int,Int,Int)
calculate_buf_sizes cb_t_maxseg seg_mss bw_delay_product_for_rt is_local_conn
  rcvbufsize sndbufsize cb_tf_doing_tstmp
    = let t_maxseg' =
           let maxseg = (min cb_t_maxseg (max 64 $ (case seg_mss of Nothing -> mssdflt; Just x-> x))) in
              -- BSD
              maxseg - (calculate_tcp_options_len cb_tf_doing_tstmp)
      in
      let t_maxseg'' = rounddown mclbytes (t_maxseg') in
      let rcvbufsize' = case bw_delay_product_for_rt of Nothing->rcvbufsize; Just x->x in
      let (rcvbufsize'', t_maxseg''') = ( if rcvbufsize' < t_maxseg''
                                             then (rcvbufsize', rcvbufsize')
                                             else (min (sb_max) (roundup (t_maxseg'') rcvbufsize'), 
                                                   t_maxseg'')) in 
      let sndbufsize' = case bw_delay_product_for_rt of Nothing->sndbufsize; Just x->x in
      let sndbufsize'' = (if sndbufsize' < t_maxseg'''
                             then sndbufsize'
                             else min (sb_max) (roundup (t_maxseg'') sndbufsize')) in
      let snd_cwnd' = t_maxseg''' * ((if is_local_conn then ss_fltsz_local else ss_fltsz)) in
      (rcvbufsize'', sndbufsize'', t_maxseg''', snd_cwnd')


calculate_bsd_rcv_wnd :: TCPSocket t -> Int
calculate_bsd_rcv_wnd tcp_sock =
  let cb' = cb_rcv tcp_sock
   in assert (rcv_adv cb' >= rcv_nxt cb') $ -- assertion for debugging
        max (seq_diff (rcv_adv cb') (rcv_nxt cb'))
          (freebsd_so_rcvbuf - bufc_length (rcvq cb'))

send_queue_space :: Num a => a -> a -> a
send_queue_space sndq_max sndq_size = (sndq_max - sndq_size)



update_idle :: Time -> TCPSocket threadt -> (Maybe Time, Maybe Time)
update_idle curr_time tcp_sock =
    let tt_keep' = if not (st tcp_sock == SYN_RECEIVED && tf_needfin (cb tcp_sock)) then
                      Just (slow_timer curr_time tcptv_keep_idle)
                   else
                      tt_keep $ cb_time tcp_sock
        tt_fin_wait_2' = if st tcp_sock == FIN_WAIT_2 then
                            Just (slow_timer curr_time tcptv_maxidle )
                         else
                            tt_fin_wait_2 $ cb_time tcp_sock
    in
    (tt_keep', tt_fin_wait_2')

-- tcp timing and rtt


tcp_backoffs :: [Int]
tcp_backoffs  = tcp_bsd_backoffs

tcp_syn_backoffs :: a
tcp_syn_backoffs  = tcp_syn_backoffs

mode_of :: Maybe (Timed (RexmtMode,Int)) -> Maybe RexmtMode
mode_of  = fmap unwrap
  where
  unwrap (Timed (x,_) _) = x

-- TODO: not sure if 0 is a suitable default
shift_of :: Maybe (Timed (RexmtMode,Int)) -> Int
shift_of  = maybe 0 unwrap
  where
  unwrap (Timed (_,s) _) = s

-- todo: check types!

-- compute the retransmit timeout to use
computed_rto :: [Int] -> Int -> Rttinf -> Time
computed_rto backoffs s ri =
    to_Int64 (backoffs !! s ) * max (t_rttmin ri) (t_srtt ri + 4 * t_rttvar ri)

-- compute the last-used rxtcur
computed_rxtcur :: Rttinf -> Time
computed_rxtcur ri
  = max (t_rttmin ri)
  $ min (tcptv_rexmtmax)
  $ computed_rto
    (if t_wassyn ri then tcp_syn_backoffs else tcp_backoffs)
    (t_lastshift ri) ri

start_tt_rexmt_gen :: RexmtMode -> [Int] -> Int -> Bool -> Rttinf -> Time
                   -> Maybe (Timed (RexmtMode,Int))
start_tt_rexmt_gen mode backoffs s wantmin ri curr_time =
    let rxtcur = max (if wantmin
                         then max (t_rttmin ri) (t_lastrtt ri + (2*1000*1000 `div` 100)) -- 2s/100
                         else t_rttmin ri )
                      ( min (tcptv_rexmtmax )
                            ( computed_rto backoffs s ri) )
    in
    Just ( Timed (mode,s) (create_timer curr_time rxtcur ) )

start_tt_rexmt :: Int -> Bool -> Rttinf -> Time -> Maybe (Timed (RexmtMode,Int))
start_tt_rexmt  = start_tt_rexmt_gen Rexmt tcp_backoffs

start_tt_rexmtsyn :: Int -> Bool -> Rttinf -> Time
                  -> Maybe (Timed (RexmtMode,Int))
start_tt_rexmtsyn  = start_tt_rexmt_gen RexmtSyn tcp_syn_backoffs

start_tt_persist :: Int -> Rttinf -> Time -> Maybe (Timed (RexmtMode,Int))
start_tt_persist s ri curr_time =
    let cur = max (tcptv_persmin)
                  (min (tcptv_persmax)
                       (computed_rto tcp_backoffs s ri) )
     in Just ( Timed (Persist, s) (create_timer curr_time cur))

update_rtt :: Time -> Rttinf -> Rttinf
update_rtt rtt ri =
    let (t_srtt'', t_rttvar'')
            = if tf_srtt_valid ri then
                 let delta = (rtt - 1000*10) - (t_srtt ri)  -- 1000*10 = 1/HZ
                     vardelta = (abs delta) - (t_rttvar ri)
                     t_srtt' = max (1000*1000 `div` (32*100)) (t_srtt ri + (delta `div` 8))
                     t_rttvar'=max (1000*1000 `div` (16*100)) (t_rttvar ri + (vardelta `div` 4))
                 in (t_srtt', t_rttvar')
                else
                 let t_srtt' = rtt
                     t_rttvar' = rtt `div` 2
                 in  (t_srtt',t_rttvar')
    in
     ri { t_rttupdated = t_rttupdated ri + 1
        , tf_srtt_valid = True
        , t_srtt = t_srtt''
        , t_rttvar = t_rttvar''
        , t_lastrtt = rtt
        , t_lastshift = 0
        , t_wassyn = False
        }

expand_cwnd :: Integral a => a -> a -> a -> a -> a
expand_cwnd ssthresh maxseg maxwin cwnd
    = min maxwin (cwnd + (if cwnd > ssthresh then (maxseg * maxseg) `div` cwnd else maxseg))

-- Path MTU Discovery

--mtu_tab  = [65535, 32000, 17914, 8166, 4352, 2002, 1492, 1006, 508, 296, 88]

next_smaller :: [Int] -> Int -> Int
next_smaller xs0 value = loop xs0
  where
  loop xs = case xs of
    x:xs' | value >= x -> x
          | otherwise  -> loop xs'
    -- TODO: Not sure if this is an acceptable default value
    []                 -> value


initial_cb_time :: TCBTiming
initial_cb_time  = TCBTiming
    { tt_keep        = Nothing
    , tt_conn_est    = Nothing
    , tt_fin_wait_2  = Nothing
    , tt_2msl        = Nothing
    , t_idletime     = 0
    , ts_recent      = Nothing
    , t_badrxtwin    = Nothing
    }

initial_cb_snd :: TCBSending
initial_cb_snd  = TCBSending
    { sndq            = bufferchain_empty
    , snd_una         = SeqLocal 0
    , snd_wnd         = 0
    , snd_wl1         = SeqForeign 0
    , snd_wl2         = SeqLocal 0
    , snd_cwnd        = tcp_maxwin `shiftL` tcp_maxwinscale
    , snd_nxt         = SeqLocal 0
    , snd_max         = SeqLocal 0
    , t_dupacks       = 0
    , t_rttinf        = Rttinf { t_rttupdated = 0
                               , tf_srtt_valid = False
                               , t_srtt = tcptv_rtobase
                               , t_rttvar = tcptv_rttvarbase
                               , t_rttmin = tcptv_min
                               , t_lastrtt = 0
                               , t_lastshift = 0
                               , t_wassyn = False
                               }
    , t_rttseg        = Nothing
    , tt_rexmt        = Nothing
    }

{-# INLINE hasfin #-}
{-# INLINE tcp_reass #-}
{-# INLINE tcp_reass_prune #-}

hasfin :: TCPReassSegment -> Int
hasfin seg
  | trs_FIN seg = 1
  | otherwise   = 0

-- returns (1) the string 
--         (2) the SEQ for the next byte
--         (3) whether FIN has been reached
--         (4) remaining...
-- this is a very SLOW algorithm and should be replaced ....

tcp_reass :: SeqForeign -> [TCPReassSegment]
          -> (BufferChain, SeqForeign, Bool, [TCPReassSegment])
tcp_reass sq rsegq =
  let searchpkt rseg = 
         let sq1 = (trs_seq rseg)
             sq2 = sq1 `seq_plus` fromIntegral (bufc_length (trs_data rseg))
                       `seq_plus` fromIntegral (hasfin rseg)
         in sq >= sq1 && sq < sq2
  in
  case List.find searchpkt rsegq of
    Nothing   -> (bufferchain_empty, sq, False, rsegq)
    Just rseg ->
      let data_to_trim = sq `seq_diff` (trs_seq rseg)
          result_buf   = bufferchain_drop data_to_trim (trs_data rseg)
          next_seq     = trs_seq rseg
              `seq_plus` fromIntegral (bufc_length (trs_data rseg))
              `seq_plus` fromIntegral (hasfin rseg)
          new_rsegq  = tcp_reass_prune next_seq rsegq
       in if trs_FIN rseg
             then (result_buf, next_seq, True, new_rsegq)
             else let (bufc2, next_seq2, hasfin2, rsegq2) =
                          tcp_reass next_seq new_rsegq
                  in ( bufferchain_concat result_buf bufc2
                     , next_seq2
                     , hasfin2
                     , rsegq2
                     )

tcp_reass_prune :: SeqForeign -> [TCPReassSegment] -> [TCPReassSegment]
tcp_reass_prune sq = List.filter p
  where
  p seg = nxtseq > sq
    where
    nxtseq = trs_seq seg `seq_plus` fromIntegral (bufc_length (trs_data seg))
                         `seq_plus` fromIntegral (hasfin seg)

initial_cb_rcv :: TCBReceiving
initial_cb_rcv  = TCBReceiving
    { last_ack_sent   = SeqForeign 0
    , tf_rxwin0sent   = False
    , tf_shouldacknow = False
    , tt_delack       = False
    , rcv_adv         = SeqForeign 0
    , rcv_wnd         = 0
    , rcv_nxt         = SeqForeign 0
    , rcvq            = bufferchain_empty
    , t_segq          = []
    }

initial_cb_misc :: TCBMisc
initial_cb_misc  = TCBMisc
    { -- retransmission
      snd_ssthresh      = tcp_maxwin `shiftL` tcp_maxwinscale
    , snd_cwnd_prev     = 0
    , snd_ssthresh_prev = 0
    , snd_recover       = SeqLocal 0
    -- some tags
    , cantsndmore       = False
    , cantrcvmore       = False
    , bsd_cantconnect   = False
    -- initialization parameters
    , self_id           = SocketID (0,TCPAddr (IPAddr 0,0))
    , parent_id         = SocketID (0,TCPAddr (IPAddr 0,0))
    , local_addr        = TCPAddr (IPAddr 0,0)
    , remote_addr       = TCPAddr (IPAddr 0,0)
    , t_maxseg          = mssdflt
    , t_advmss          = Nothing
    , tf_doing_ws       = False    
    , tf_doing_tstmp    = False
    , tf_req_tstmp      = False
    , request_r_scale   = Nothing
    , snd_scale         = 0
    , rcv_scale         = 0
    , iss               = SeqLocal 0
    , irs               = SeqForeign 0
    -- other things i don't use for the moment
    , sndurp            = Nothing
    , rcvurp            = Nothing
    , iobc              = NO_OOBDATA
    , rcv_up            = SeqForeign 0
    , tf_needfin        = False
    }

initial_tcp_socket :: TCPSocket threadt
initial_tcp_socket  = TCPSocket
    { st      = CLOSED
    , cb_time = initial_cb_time
    , cb_snd  = initial_cb_snd
    , cb_rcv  = initial_cb_rcv
    , cb      = initial_cb_misc
    , sock_listen  = SocketListen [] [] 0
    , waiting_list = []
    }

empty_sid :: SocketID
empty_sid = SocketID (0,TCPAddr (IPAddr 0,0))


