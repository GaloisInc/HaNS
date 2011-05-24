{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP              #-}

module Hans.Channel where

#ifdef BOUNDED_CHANNELS
import Control.Concurrent.BoundedChan as C

type Channel a = BoundedChan a

newChannel :: IO (Channel a)
newChannel  = newBoundedChan 20

receive :: Channel a -> IO a
receive c = readChan c

send :: Channel a -> a -> IO ()
send c a = writeChan c $! a

#else
import Control.Concurrent.Chan as C

type Channel a = Chan a

newChannel :: IO (Channel a)
newChannel  = newChan

receive :: Channel a -> IO a
receive  = readChan

send :: Channel a -> a -> IO ()
send c a = writeChan c $! a

#endif
