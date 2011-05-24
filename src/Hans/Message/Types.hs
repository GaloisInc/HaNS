{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hans.Message.Types where

import Data.Serialize (Serialize)
import Data.Word (Word16)

newtype Lifetime = Lifetime Word16
  deriving (Show,Eq,Ord,Num,Serialize)
