module Hans.Config where


-- | General network stack configuration.
data Config = Config { cfgInputQueueSize :: {-# UNPACK #-} !Int

                     , cfgArpTableSize :: {-# UNPACK #-} !Int
                       -- ^ Best to pick a prime number.
                     }

defaultConfig :: Config
defaultConfig  = Config { cfgInputQueueSize = 128
                        , cfgArpTableSize   = 67
                        }
