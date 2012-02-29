{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
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

module Network.TCP.Type.Base where

import Data.Time.Clock.POSIX (POSIXTime,getPOSIXTime)
import Foreign
import Control.Exception
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import System.Random


to_Int :: Integral a => a -> Int
to_Int x = fromIntegral x

to_Int8 :: Integral a => a -> Int8
to_Int8 x = fromIntegral x

to_Int16 :: Integral a => a -> Int16
to_Int16 x  = fromIntegral x

to_Int32 :: Integral a => a -> Int32
to_Int32 x  = fromIntegral x

to_Int64 :: Integral a => a -> Int64
to_Int64 x  = fromIntegral x

to_Word :: Integral a => a -> Word
to_Word x   = (fromIntegral x)

to_Word8 :: Integral a => a -> Word8
to_Word8 x  = (fromIntegral x)

to_Word16 :: Integral a => a -> Word16
to_Word16 x = (fromIntegral x)

to_Word32 :: Integral a => a -> Word32
to_Word32 x = (fromIntegral x)

to_Word64 :: Integral a => a -> Word64
to_Word64 x = (fromIntegral x)


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
get_ip (TCPAddr (i,_)) = i

get_port :: TCPAddr -> Port
get_port (TCPAddr (_,p)) = p

get_remote_addr :: SocketID -> TCPAddr
get_remote_addr (SocketID (_,a)) = a

get_local_port :: SocketID -> Port
get_local_port (SocketID (p,_)) = p

{-# INLINE get_ip #-}
{-# INLINE get_port #-}
{-# INLINE get_remote_addr #-}
{-# INLINE get_local_port #-}

-- TCP Sequence numbers

class (Eq a) => Seq32 a where
  seq_val   :: a -> Word32
  seq_lt    :: a -> a -> Bool
  seq_leq   :: a -> a -> Bool
  seq_gt    :: a -> a -> Bool
  seq_geq   :: a -> a -> Bool
  seq_plus  :: a -> Word32 -> a
  seq_minus :: a -> Word32 -> a
  seq_diff  :: a -> a -> Int

infixl 6 `seq_plus`

instance Seq32 Word32 where
  seq_val w = w
  {-# INLINE seq_val #-}

  seq_lt  x y = x < y
  {-# INLINE seq_lt  #-}

  seq_leq x y = x <= y
  {-# INLINE seq_leq #-}

  seq_gt  x y = x > y
  {-# INLINE seq_gt  #-}

  seq_geq x y = x >= y
  {-# INLINE seq_geq #-}

  seq_plus  s i = assert (i>=0)  (s + i)
  {-# INLINE seq_plus #-}

  seq_minus s i = assert (i>=0)  (s - i)
  {-# INLINE seq_minus  #-}

  seq_diff s t = assert (res>=0) res
    where
    res = fromIntegral s - fromIntegral t
  {-# INLINE seq_diff #-}

#if defined(xen_HOST_OS)
instance Random Word32 where
  randomR (l,h) g = let l' :: Integer = fromIntegral l
                        h'            = fromIntegral h
                        (r, g')       = randomR (l',h') g
                    in (fromIntegral r, g')
  random          = randomR (minBound, maxBound)
#endif

newtype SeqLocal   = SeqLocal   Word32 deriving (Eq,Show,Seq32,Random)
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

seq_flip_ltof :: SeqLocal -> SeqForeign
seq_flip_ltof (SeqLocal w) = SeqForeign w
{-# INLINE seq_flip_ltof  #-}

seq_flip_ftol :: SeqForeign -> SeqLocal
seq_flip_ftol (SeqForeign w) = SeqLocal w
{-# INLINE seq_flip_ftol  #-}

fseq_val :: SeqForeign -> Word32
fseq_val (SeqForeign w32) = w32



-- | Clock time, in microseconds.
type Time = Int64

seconds_to_time :: Double -> Time
seconds_to_time f = round f * 1000 * 1000

{-# INLINE seconds_to_time  #-}

get_current_time :: IO Time
get_current_time = posixtime_to_time `fmap` getPOSIXTime

posixtime_to_time :: POSIXTime -> Time
posixtime_to_time p = round p * 1000 * 1000

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

buffer_take :: Int -> Buffer -> Buffer
buffer_take = S.take

buffer_drop :: Int -> Buffer -> Buffer
buffer_drop = S.drop

buffer_merge :: Buffer -> Buffer -> [Buffer]
buffer_merge bs1 bs2
  | S.length bs1 == 0 = [bs2]
  | S.length bs2 == 0 = [bs1]
  | otherwise         = [bs1,bs2]


type BufferChain = L.ByteString

bufc_length :: BufferChain -> Int
bufc_length  = fromIntegral . L.length

bufferchain_empty :: BufferChain
bufferchain_empty = L.empty

bufferchain_singleton :: Buffer -> BufferChain
bufferchain_singleton b = L.fromChunks [b]

bufferchain_add :: Buffer -> BufferChain -> BufferChain
bufferchain_add bs bc = bufferchain_singleton bs `L.append` bc

bufferchain_get :: BufferChain -> Int -> BufferChain
bufferchain_get bc ix = L.take 1 (L.drop (fromIntegral ix) bc)

bufferchain_append :: BufferChain -> Buffer -> BufferChain
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
