module Hans.Nat where

import           Hans.Addr (toAddr)
import qualified Hans.Nat.State as Nat
import           Hans.Network (Network)
import           Hans.Tcp.Packet (TcpPort)
import           Hans.Types
import           Hans.Udp.Packet (UdpPort)


-- | Add a TCP port-forwarding rule.
forwardTcpPort :: Network addr
               => NetworkStack
               -> addr    -- ^ Local address (can be wildcard)
               -> TcpPort -- ^ Local port
               -> addr    -- ^ Remote address
               -> TcpPort -- ^ Remote port
               -> IO ()
forwardTcpPort ns src srcPort dest destPort =
  Nat.addTcpPortForward ns
    Nat.PortForward { pfSourceAddr = toAddr src
                    , pfSourcePort = srcPort
                    , pfDestAddr   = toAddr dest
                    , pfDestPort   = destPort }

-- | Remove a TCP port-forwarding rule.
removeTcpPortForward :: Network addr
                     => NetworkStack
                     -> addr    -- ^ Local address (can be wildcard)
                     -> TcpPort -- ^ Local port
                     -> IO ()
removeTcpPortForward ns src port = Nat.removeTcpPortForward ns (toAddr src) port

-- | Add a UDP port-forwarding rule.
forwardUdpPort :: Network addr
               => NetworkStack
               -> addr    -- ^ Local address (can be wildcard)
               -> UdpPort -- ^ Local port
               -> addr    -- ^ Remote address
               -> UdpPort -- ^ Remote port
               -> IO ()
forwardUdpPort ns src srcPort dest destPort =
  Nat.addUdpPortForward ns
    Nat.PortForward { pfSourceAddr = toAddr src
                    , pfSourcePort = srcPort
                    , pfDestAddr   = toAddr dest
                    , pfDestPort   = destPort }

-- | Remove a UDP port-forwarding rule.
removeUdpPortForward :: Network addr
                     => NetworkStack
                     -> addr    -- ^ Local address (can be wildcard)
                     -> UdpPort -- ^ Local port
                     -> IO ()
removeUdpPortForward ns src port = Nat.removeUdpPortForward ns (toAddr src) port
