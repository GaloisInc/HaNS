{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Hans.Socket (
    -- * Abstract Sockets
    Socket(..),
    ListenSocket(..),
    DataSocket(..),
    SocketConfig(..), defaultSocketConfig,
    SockPort,

    -- ** UDP Sockets
    UdpSocket(),
    newUdpSocket,
    sendto,
    recvfrom,

    -- ** TCP Sockets
    TcpSocket(),
    TcpListenSocket(),

    tcpRemoteAddr,
    tcpRemotePort,
    tcpLocalAddr,
    tcpLocalPort

  ) where

import Hans.Socket.Udp
import Hans.Socket.Tcp
import Hans.Socket.Types
