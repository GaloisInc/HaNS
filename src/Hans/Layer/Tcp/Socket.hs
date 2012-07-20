module Hans.Layer.Tcp.Socket (
    Socket()
  , listen
  ) where

import Hans.Address.IP4
import Hans.Channel
import Hans.Layer
import Hans.Layer.Tcp.Connection
import Hans.Layer.Tcp.Monad
import Hans.Message.Tcp

import Control.Concurrent (MVar,newEmptyMVar,takeMVar,putMVar)


-- Socket Interface ------------------------------------------------------------

data Socket = Socket
  { sockHandle :: TcpHandle
  , sockIdent  :: !ListenConnection
  }

data SocketResult a
  = SocketResult a
  | SocketError
    deriving (Show)

blockResult :: TcpHandle -> (MVar (SocketResult a) -> Tcp ()) -> IO a
blockResult tcp action = do
  res     <- newEmptyMVar
  send tcp (action res)
  sockRes <- takeMVar res
  case sockRes of
    SocketResult a -> return a
    SocketError    -> fail "SocketError"


listen :: TcpHandle -> IP4 -> TcpPort -> IO Socket
listen tcp src port = blockResult tcp $ \ res -> do
  ls <- getListenConnections
  case lookupListeningConnection src port ls of

    Nothing -> do
      let lc = ListenConnection { lcHost = Just src }
      setListenConnections (addListenConnection port lc ls)
      output $ putMVar res $ SocketResult Socket
        { sockHandle = tcp
        , sockIdent  = lc
        }

    Just _ -> output (putMVar res SocketError)
