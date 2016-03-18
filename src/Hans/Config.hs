module Hans.Config (
    Config(..),
    defaultConfig,
    HasConfig(..),
  ) where

import Hans.Lens

import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word8)


-- | General network stack configuration.
data Config = Config { cfgInputQueueSize :: !Int

                     , cfgArpTableSize :: !Int
                       -- ^ Best to pick a prime number.

                     , cfgArpTableLifetime :: !NominalDiffTime

                     , cfgArpRetry :: !Int
                       -- ^ Number of times to retry an arp request before
                       -- failing

                     , cfgArpRetryDelay :: !Int
                       -- ^ The amount of time to wait between arp request
                       -- retransmission.

                     , cfgIP4FragTimeout :: !NominalDiffTime
                       -- ^ Number of seconds to wait before expiring a
                       -- partially reassembled IP4 packet

                     , cfgIP4InitialTTL :: !Word8

                     , cfgIP4MaxFragTableEntries :: !Int
                       -- ^ Maximum packets being reassembled at any time.

                     , cfgUdpSocketTableSize :: !Int
                       -- ^ Number of buckets in the udp socket table

                     , cfgDnsResolveTimeout :: !Int
                       -- ^ In microseconds

                     , cfgTcpListenTableSize :: !Int
                       -- ^ Small-ish prime number

                     , cfgTcpActiveTableSize :: !Int
                       -- ^ Best to pick a prime number

                     , cfgTcpTimeoutTimeWait :: !NominalDiffTime
                       -- ^ Time to remain in TimeWait, in seconds.

                     , cfgTcpInitialMSS :: !Int
                       -- ^ Initial MSS for tcp connections

                     , cfgTcpMaxSynBacklog :: !Int
                       -- ^ Maximum number of connections waiting for an
                       -- acknowledgement.

                     , cfgTcpInitialWindow :: !Int
                       -- ^ Initial local window for tcp connections.

                     , cfgTcpMSL :: !Int
                       -- ^ Maximum segment lifetime

                     , cfgTcpTSClockFrequency :: !NominalDiffTime
                       -- ^ Frequency (in Hz) of timestamp clock updates.
                       -- Should be between 1Hz and 1000Hz, according to
                       -- RFC-1323.

                     , cfgTcpTimeWaitSocketLimit :: !Int
                       -- ^ The max number of threads allowed in the time
                       -- wait heap.
                     }

defaultConfig :: Config
defaultConfig  = Config { cfgInputQueueSize     = 128
                        , cfgArpTableSize       = 67
                        , cfgArpTableLifetime   = 60 -- 60 seconds
                        , cfgArpRetry           = 10
                        , cfgArpRetryDelay      = 2000 -- 2 seconds
                        , cfgIP4FragTimeout     = 30
                        , cfgIP4InitialTTL      = 128
                        , cfgIP4MaxFragTableEntries= 32
                        , cfgUdpSocketTableSize = 31
                        , cfgDnsResolveTimeout  = 5000000
                        , cfgTcpListenTableSize = 5
                        , cfgTcpActiveTableSize = 67
                        , cfgTcpTimeoutTimeWait = 60.0 -- one minute
                        , cfgTcpInitialMSS      = 512
                        , cfgTcpMaxSynBacklog   = 128
                        , cfgTcpInitialWindow   = 14600
                        , cfgTcpMSL             = 60 -- one minute
                        , cfgTcpTSClockFrequency = 1000 -- 1000hz, update every 1ms
                        , cfgTcpTimeWaitSocketLimit = 70
                        }

class HasConfig cfg where
  config :: Getting r cfg Config

instance HasConfig Config where
  config = id
  {-# INLINE config #-}

