{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Hans.Addr.Types where

import Hans.IP4.Packet (IP4)

import Data.Hashable (Hashable)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

data Addr = Addr4 !IP4
            deriving (Eq,Ord,Show,Generic,Typeable)

instance Hashable Addr


sameFamily :: Addr -> Addr -> Bool
sameFamily Addr4{} Addr4{} = True
