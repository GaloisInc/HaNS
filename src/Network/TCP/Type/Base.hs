{-# OPTIONS_GHC -fglasgow-exts #-}

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

module Network.TCP.Type.Base where

import Data.Time.Clock.POSIX (POSIXTime,getPOSIXTime)
import Foreign
import Foreign.C
import System.IO.Unsafe
import Control.Exception
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L


to_Int x    = (fromIntegral x)::Int
to_Int8  x  = (fromIntegral x)::Int8
to_Int16 x  = (fromIntegral x)::Int16
to_Int32 x  = (fromIntegral x)::Int32
to_Int64 x  = (fromIntegral x)::Int64

to_Word x   = (fromIntegral x)::Word
to_Word8 x  = (fromIntegral x)::Word8
to_Word16 x = (fromIntegral x)::Word16
to_Word32 x = (fromIntegral x)::Word32
to_Word64 x = (fromIntegral x)::Word64


{-# INLINE to_Int    #-}
{-# INLINE to_Int8   #-}
{-# INLINE to_Int16  #-}
{-# INLINE to_Int32  #-}
{-# INLINE to_Int64  #-}
{-# INLINE to_Word   #-}
{-# INLINE to_Word8  #-}
{-# INLINE to_Word16 #-}
{-# INLINE to_Word32 #-}
{-# INLINE to_Word64 #-}

-- Port numbers, IP addresses

type Port = Word16
newtype IPAddr   = IPAddr   Word32          deriving (Eq,Ord)
newtype TCPAddr  = TCPAddr  (IPAddr, Port)  deriving (Eq,Ord)
newtype SocketID = SocketID (Port, TCPAddr) deriving (Eq,Ord,Show)

instance Show IPAddr where
 show (IPAddr w) = (show $ w .&. 255) ++ "." ++
                   (show $ (w `shiftR` 8)  .&. 255) ++ "." ++
                   (show $ (w `shiftR` 16) .&. 255) ++ "." ++
                   (show $ (w `shiftR` 24) .&. 255)
instance Show TCPAddr where
 show (TCPAddr (ip,pt)) = (show ip) ++ ":" ++ (show pt)


get_ip :: TCPAddr -> IPAddr
get_ip (TCPAddr (i,p)) = i

get_port :: TCPAddr -> Port
get_port (TCPAddr (i,p)) = p

get_remote_addr :: SocketID -> TCPAddr
get_remote_addr (SocketID (p,a)) = a

get_local_port :: SocketID -> Port
get_local_port (SocketID (p,a)) = p

{-# INLINE get_ip #-}
{-# INLINE get_port #-}
{-# INLINE get_remote_addr #-}
{-# INLINE get_local_port #-}

-- TCP Sequence numbers

class (Eq a) => Seq32 a where
  seq_val :: a -> Word32
  seq_lt  :: a -> a -> Bool
  seq_leq :: a -> a -> Bool
  seq_gt  :: a -> a -> Bool
  seq_geq :: a -> a -> Bool
  seq_plus  :: (Integral n) => a -> n -> a
  seq_minus :: (Integral n) => a -> n -> a
  seq_diff  :: (Integral n) => a -> a -> n

instance Seq32 Word32 where
  seq_val w = w
  seq_lt  x y = (to_Int32 (x-y)) <  0
  seq_leq x y = (to_Int32 (x-y)) <= 0
  seq_gt  x y = (to_Int32 (x-y)) >  0
  seq_geq x y = (to_Int32 (x-y)) >= 0
  seq_plus  s i = assert (i>=0)  $ s + (to_Word32 i)
  seq_minus s i = assert (i>=0)  $ s - (to_Word32 i)
  seq_diff  s t = let res=fromIntegral $ to_Int32 (s-t) in assert (res>=0) res
  {-# INLINE seq_val #-}
  {-# INLINE seq_lt  #-}
  {-# INLINE seq_leq #-}
  {-# INLINE seq_gt  #-}
  {-# INLINE seq_geq #-}
  {-# INLINE seq_plus #-}
  {-# INLINE seq_minus  #-}
  {-# INLINE seq_diff #-}

newtype SeqLocal   = SeqLocal   Word32 deriving (Eq,Show,Seq32)
newtype SeqForeign = SeqForeign Word32 deriving (Eq,Show,Seq32)
newtype Timestamp  = Timestamp  Word32 deriving (Eq,Show,Seq32)

instance Ord SeqLocal where
  (<) = seq_lt
  (>) = seq_gt
  (<=) = seq_leq
  (>=) = seq_geq
  {-# INLINE (<) #-}  
  {-# INLINE (>) #-}  
  {-# INLINE (<=) #-}  
  {-# INLINE (>=) #-}  
instance Ord SeqForeign where
  (<) = seq_lt
  (>) = seq_gt
  (<=) = seq_leq
  (>=) = seq_geq
  {-# INLINE (<) #-}  
  {-# INLINE (>) #-}  
  {-# INLINE (<=) #-}  
  {-# INLINE (>=) #-}  
instance Ord Timestamp where
  (<) = seq_lt
  (>) = seq_gt
  (<=) = seq_leq
  (>=) = seq_geq
  {-# INLINE (<) #-}  
  {-# INLINE (>) #-}  
  {-# INLINE (<=) #-}  
  {-# INLINE (>=) #-}  

seq_flip_ltof (SeqLocal w) = SeqForeign w
seq_flip_ftol (SeqForeign w) = SeqLocal w

fseq_val :: SeqForeign -> Word32
fseq_val (SeqForeign w32) = w32

{-# INLINE seq_flip_ltof  #-}
{-# INLINE seq_flip_ftol  #-}


-- | Clock time, in microseconds.
type Time = Int64

seconds_to_time :: RealFrac a => a -> Time
seconds_to_time f = round (f * 1000*1000)

{-# INLINE seconds_to_time  #-}

get_current_time :: IO Time
get_current_time = posixtime_to_time `fmap` getPOSIXTime

posixtime_to_time :: POSIXTime -> Time
posixtime_to_time  = seconds_to_time . toRational

---------------------------------------------------------------------------

type Buffer = S.ByteString

buf_len :: Buffer -> Int
buf_len  = S.length

buffer_ok :: Buffer -> Bool
buffer_ok _ = True

buffer_empty :: Buffer
buffer_empty  = S.empty

buffer_to_string :: Buffer -> IO String
buffer_to_string  = return . map (toEnum . fromEnum) . S.unpack

string_to_buffer :: String -> IO Buffer
string_to_buffer  = return . S.pack . map (toEnum . fromEnum)

buffer_split :: Int -> Buffer -> (Buffer,Buffer)
buffer_split  = S.splitAt

buffer_take = S.take
buffer_drop = S.drop

buffer_merge :: Buffer -> Buffer -> [Buffer]
buffer_merge bs1 bs2
  | S.length bs1 == 0 = [bs2]
  | S.length bs2 == 0 = [bs1]
  | otherwise         = [bs1,bs2]


type BufferChain = L.ByteString

bufc_length :: BufferChain -> Int
bufc_length  = fromIntegral . L.length

bufferchain_empty = L.empty
bufferchain_singleton b = L.fromChunks [b]

bufferchain_add bs bc = bufferchain_singleton bs `L.append` bc

bufferchain_get :: BufferChain -> Int -> BufferChain
bufferchain_get bc ix = L.take 1 (L.drop (fromIntegral ix) bc)

bufferchain_append bc bs = bc `L.append` bufferchain_singleton bs

bufferchain_concat :: BufferChain -> BufferChain -> BufferChain
bufferchain_concat  = L.append

bufferchain_head :: BufferChain -> Buffer
bufferchain_head  = head . L.toChunks

bufferchain_tail :: BufferChain -> BufferChain
bufferchain_tail  = L.fromChunks . tail . L.toChunks

bufferchain_take :: Int -> BufferChain -> BufferChain
bufferchain_take  = L.take . fromIntegral

bufferchain_drop :: Int -> BufferChain -> BufferChain
bufferchain_drop  = L.drop . fromIntegral

bufferchain_split_at :: Int -> BufferChain -> (BufferChain,BufferChain)
bufferchain_split_at  = L.splitAt . fromIntegral

bufferchain_collapse :: BufferChain -> IO Buffer
bufferchain_collapse  = return . S.concat . L.toChunks

-- bufferchain_output bc@(BufferChain lst len) (ptr::Ptr CChar) =
--   copybuf ptr lst
--   where copybuf ptrDest [] = return ()
--         copybuf ptrDest (x:xs) =
--           withForeignPtr (buf_ptr x)
--             (\ptrSrc -> do
--                  copyArray ptrDest (ptrSrc `plusPtr` (buf_offset x)) (buf_len x)
--                  copybuf (ptrDest `plusPtr` (buf_len x)) xs
--             )

bufferchain_ok :: BufferChain -> Bool
bufferchain_ok _ = True
