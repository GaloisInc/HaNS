{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent (forkIO,killThread,threadDelay)
import qualified Control.Exception as X
import           Control.Monad (replicateM,when)
import qualified Data.ByteString.Char8 as S
import qualified Data.Foldable as F
import           Network (withSocketsDo,connectTo,PortID(..))
import           System.Environment (getArgs)
import           System.IO (hPutStrLn,hGetLine,hClose)

main = withSocketsDo $
  do [addr]  <- getArgs

     X.bracket (replicateM 100 (forkClient addr))
               (mapM_ killThread) $ \ _ ->

       do _ <- getLine
          return ()

forkClient addr =
  do tid <- forkIO (createClient addr)
     threadDelay (100 * 1000)
     return tid

createClient addr =
  X.bracket (connectTo addr (PortNumber 9001)) hClose $ \ conn ->
    F.forM_ (cycle ["foo", "bar", "baz", "ASDF"]) $ \ str ->
      do S.hPutStrLn conn str
         str' <- S.hGetLine conn
         when (str /= str') $
           do putStrLn "Invalid echo server response:"
              putStr   "  expected: " >> S.putStrLn str
              putStr   "  received: " >> S.putStrLn str'
