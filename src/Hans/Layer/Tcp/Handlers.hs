module Hans.Layer.Tcp.Handlers (
    handleIncomingTcp
  ) where

import Hans.Address.IP4
import Hans.Layer.Tcp.Monad

import qualified Data.ByteString as S

handleIncomingTcp :: IP4 -> IP4 -> S.ByteString -> Tcp ()
handleIncomingTcp _src _dst _bytes = fail "handleIncomingTcp: not implemented"
