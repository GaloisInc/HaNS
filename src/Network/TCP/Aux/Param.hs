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

module Network.TCP.Aux.Param where

import Network.TCP.Type.Base

dschedmax = seconds_to_time 1
dinput_queuemax = seconds_to_time 1
doutput_queuemax = seconds_to_time 1

hz = 100
      
tickintvlmin = seconds_to_time $ 100/(105*hz)
tickintvlmax = seconds_to_time $ 105/(100*hz)

slow_timer_intvl = seconds_to_time $ 1/2
-- slow_timer_model_intvl = seconds_to_time $ 1/1000

fast_timer_intvl = seconds_to_time $ 1/5
-- fast_timer_model_intvl = seconds_to_time $ 1/1000

kern_timer_intvl = tickintvlmax
-- kern_timer_model_intvl = 


-- listen queue length
somaxconn ::Int= 128

-- buffers
mclbytes ::Int= 2048
msize ::Int= 256
sb_max ::Int= 256*1024

-- rfc limits

dtsinval::Time = seconds_to_time $ 24*24*60*60
tcp_maxwin :: Int = 65535
tcp_maxwinscale :: Int = 14


--default
freebsd_so_rcvbuf :: Int = 42080
freebsd_so_sndbuf :: Int = 9216

-- tcp parameters

mssdflt :: Int = 1400 -- 512
ss_fltsz_local :: Int = 4
ss_fltsz :: Int = 1
tcp_do_newreno = True
tcp_q0minlimit :: Int = 30
tcp_q0maxlimit :: Int = 512*30

backlog_fudge :: Int -> Int
backlog_fudge n = min somaxconn n

-- time values (TCP only)

tcptv_delack     = seconds_to_time 0.1
tcptv_rtobase    = seconds_to_time 3
tcptv_rttvarbase = seconds_to_time 0
tcptv_min        = seconds_to_time 1
tcptv_rexmtmax   = seconds_to_time 64
tcptv_msl        = seconds_to_time 1 -- this is too stringent... good for testing
tcptv_persmin    = seconds_to_time 5
tcptv_persmax    = seconds_to_time 60
tcptv_keep_init  = seconds_to_time 75
tcptv_keep_idle  = seconds_to_time $ 120*60
tcptv_keepintvl  = seconds_to_time $ 75
tcptv_keepcnt    = seconds_to_time $ 8
tcptv_maxidle    = tcptv_keepintvl*8


-- timing related parameters (TCP only)

tcp_bsd_backoffs   :: [Int]= [1,2,4,8,16,32,64,  64,64,64, 64,64,64]
tcp_linux_backoffs = [1,2,4,8,16,32,64,  128,256,512, 512]
tcp_winxp_backoffs = [1,2,4,8,16]

--tcp_maxrxtshift = 12
tcp_maxrxtshift :: Int = 3 -- this is not right... for testing only
tcp_synackmaxrxtshift :: Int = 3

tcp_syn_bsd_backoffs :: [Int] = [1,1,1,1,1,2,4,8,16,32,64,64,64]
tcp_syn_linux_backoffs =[1,2,4,8,16]
tcp_syn_winxp_backoffs=[1,2]

listen_qlimit :: Int = 100
