{-# LANGUAGE RecordWildCards #-}

module Hans.IP4 (
    module Exports,
    module Hans.IP4
  ) where

import Hans.IP4.Types as Exports
import Hans.Monad (Hans)

import qualified Data.ByteString as S


processIP4 :: IP4State -> S.ByteString -> Hans ()
processIP4 IP4State payload =
  undefined
