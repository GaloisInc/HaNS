{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hans.Message.Types where

import Control.Applicative ((<$>))
import Data.Serialize.Get (Get,getWord16be)
import Data.Serialize.Put (Putter,putWord16be)
import Data.Word (Word16)

newtype Lifetime = Lifetime { getLifetime :: Word16 }
  deriving (Show,Eq,Ord,Num)

parseLifetime :: Get Lifetime
parseLifetime  = Lifetime <$> getWord16be

renderLifetime :: Putter Lifetime
renderLifetime  = putWord16be . getLifetime
