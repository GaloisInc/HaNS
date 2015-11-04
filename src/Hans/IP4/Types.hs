{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.IP4.Types where

import           Data.Hashable (Hashable)
import           Data.Serialize.Get (Get,getWord32be)
import           Data.Serialize.Put (Putter,putWord32be)
import           Data.Word (Word32)


-- IP4 Addresses ---------------------------------------------------------------

newtype IP4 = IP4 { ip4Bytes :: Word32
                  } deriving (Eq,Ord,Show,Hashable)

getIP4 :: Get IP4
getIP4  =
  do w <- getWord32be
     return (IP4 w)

putIP4 :: Putter IP4
putIP4 (IP4 w) = putWord32be w
