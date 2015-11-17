module Hans.Config (
    Config(..),
    defaultConfig,
    HasConfig(..),
  ) where

import Data.Time.Clock (NominalDiffTime)


-- | General network stack configuration.
data Config = Config { cfgInputQueueSize :: {-# UNPACK #-} !Int

                     , cfgArpTableSize :: {-# UNPACK #-} !Int
                       -- ^ Best to pick a prime number.

                     , cfgArpTableLifetime :: !NominalDiffTime

                     , cfgArpRetry :: {-# UNPACK #-} !Int
                       -- ^ Number of times to retry an arp request before
                       -- failing

                     , cfgArpRetryDelay :: {-# UNPACK #-} !Int
                       -- ^ The amount of time to wait between arp request
                       -- retransmission.

                     , cfgIP4FragTimeout :: !NominalDiffTime
                     }

defaultConfig :: Config
defaultConfig  = Config { cfgInputQueueSize   = 128
                        , cfgArpTableSize     = 67
                        , cfgArpTableLifetime = 60 -- 60 seconds
                        , cfgArpRetry         = 10
                        , cfgArpRetryDelay    = 2000 -- 2 seconds
                        , cfgIP4FragTimeout   = 30
                        }

class HasConfig cfg where
  getConfig :: cfg -> Config

instance HasConfig Config where
  getConfig = id
  {-# INLINE getConfig #-}

