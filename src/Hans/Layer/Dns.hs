module Hans.Layer.Dns (
    DnsHandle
  , runDnsLayer
  ) where

import Hans.Channel
import Hans.Layer
import Hans.Layer.Udp

import Control.Concurrent (forkIO)


-- External Interface ----------------------------------------------------------

type DnsHandle = Channel (Dns ())

runDnsLayer :: DnsHandle -> UdpHandle -> IO ()
runDnsLayer h udp =
  do _ <- forkIO (loopLayer "dns" (emptyDnsState udp) (receive h) id)
     return ()


-- Handlers --------------------------------------------------------------------

type Dns = Layer DnsState

data DnsState = DnsState { dnsUdpHandle :: UdpHandle }

emptyDnsState :: UdpHandle -> DnsState
emptyDnsState h = DnsState { dnsUdpHandle = h }
