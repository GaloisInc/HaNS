module Hans.Nat where

import           Hans.Addr (IP4)
import qualified Hans.Nat.State as Nat
import           Hans.Tcp.Packet (TcpPort)
import           Hans.Types
import           Hans.Udp.Packet (UdpPort)


-- | Add a TCP port-forwarding rule.
forwardTcpPort :: NetworkStack
               -> IP4     -- ^ Local address (can be wildcard)
               -> TcpPort -- ^ Local port
               -> IP4     -- ^ Remote address
               -> TcpPort -- ^ Remote port
               -> IO ()
forwardTcpPort ns src srcPort dest destPort =
  Nat.addTcpPortForward ns
    Nat.PortForward { pfSourceAddr = src
                    , pfSourcePort = srcPort
                    , pfDestAddr   = dest
                    , pfDestPort   = destPort }

-- | Remove a TCP port-forwarding rule.
removeTcpPortForward :: NetworkStack
                     -> IP4     -- ^ Local address (can be wildcard)
                     -> TcpPort -- ^ Local port
                     -> IO ()
removeTcpPortForward ns src port = Nat.removeTcpPortForward ns src port

-- | Add a UDP port-forwarding rule.
forwardUdpPort :: NetworkStack
               -> IP4     -- ^ Local address (can be wildcard)
               -> UdpPort -- ^ Local port
               -> IP4     -- ^ Remote address
               -> UdpPort -- ^ Remote port
               -> IO ()
forwardUdpPort ns src srcPort dest destPort =
  Nat.addUdpPortForward ns
    Nat.PortForward { pfSourceAddr = src
                    , pfSourcePort = srcPort
                    , pfDestAddr   = dest
                    , pfDestPort   = destPort }

-- | Remove a UDP port-forwarding rule.
removeUdpPortForward :: NetworkStack
                     -> IP4     -- ^ Local address (can be wildcard)
                     -> UdpPort -- ^ Local port
                     -> IO ()
removeUdpPortForward ns src port = Nat.removeUdpPortForward ns src port
