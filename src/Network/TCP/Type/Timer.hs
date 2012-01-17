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

module Network.TCP.Type.Timer 

where

import Network.TCP.Type.Base

import Control.Monad (guard)

data Timed a = Timed { timed_val :: a
                     , timed_exp :: Time 
                     } deriving (Show, Eq)

timed_expires :: Time -> Timed a -> Bool
timed_expires t (Timed _ tm) = t >= tm

timer_expires :: Time -> Time -> Bool
timer_expires t tm = t >= tm

maybe_timed_expires :: Time -> Maybe (Timed a) -> Bool
maybe_timed_expires _ Nothing = False
maybe_timed_expires curr_time (Just t) = timed_expires curr_time t

maybe_timer_expires :: Time -> Maybe Time -> Bool
maybe_timer_expires _ Nothing = False
maybe_timer_expires curr_time (Just t) = curr_time >= t

type TimeWindow a = Maybe (Timed a)

timewindow_open :: Time -> TimeWindow a -> Bool
timewindow_open  = maybe_timed_expires

timewindow_val :: Time -> TimeWindow a -> Maybe a
timewindow_val t mb = do
  tmd <- mb
  guard (not (timed_expires t tmd))
  return (timed_val tmd)


