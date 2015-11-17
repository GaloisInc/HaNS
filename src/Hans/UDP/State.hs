module Hans.UDP.State where

import Hans.Config


data UDPState = UDPState {
                         }

newUDPState :: IO UDPState
newUDPState  =
  do return $! UDPState
