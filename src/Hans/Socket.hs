{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Hans.Socket where


data SocketType = Stream | Datagram

data Socket (ty :: SocketType) =
  Socket {
         }
