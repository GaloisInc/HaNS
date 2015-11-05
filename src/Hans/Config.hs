module Hans.Config where

import Data.Time.Clock (NominalDiffTime)


-- | General network stack configuration.
data Config = Config { cfgInputQueueSize :: {-# UNPACK #-} !Int

                     , cfgArpTableSize :: {-# UNPACK #-} !Int
                       -- ^ Best to pick a prime number.

                     , cfgArpTableLifetime :: !NominalDiffTime
                     }

defaultConfig :: Config
defaultConfig  = Config { cfgInputQueueSize   = 128
                        , cfgArpTableSize     = 67
                        , cfgArpTableLifetime = 10 * 60 -- 10 minutes
                        }
