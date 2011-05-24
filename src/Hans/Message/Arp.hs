module Hans.Message.Arp where

import Hans.Address

import Data.Serialize (Serialize(..))
import Data.Serialize.Get (getWord8,getWord16be)
import Data.Serialize.Put (putWord16be,putWord8)
import Data.Word (Word16)


-- Arp Packets -----------------------------------------------------------------

data ArpPacket hw p = ArpPacket
  { arpHwType :: !Word16
  , arpPType  :: !Word16
  , arpOper   :: ArpOper
  , arpSHA    :: hw
  , arpSPA    :: p
  , arpTHA    :: hw
  , arpTPA    :: p
  }

-- | Decode an Arp message.
instance (Address hw, Address p) => Serialize (ArpPacket hw p) where
  get = do
    hty  <- getWord16be
    pty  <- getWord16be
    _    <- getWord8
    _    <- getWord8
    oper <- get
    sha  <- get
    spa  <- get
    tha  <- get
    tpa  <- get
    return $! ArpPacket
      { arpHwType = hty
      , arpPType  = pty
      , arpOper   = oper
      , arpSHA    = sha
      , arpSPA    = spa
      , arpTHA    = tha
      , arpTPA    = tpa
      }

  put msg = do
    putWord16be (arpHwType msg)
    putWord16be (arpPType msg)
    putWord8    (addrSize (arpSHA msg))
    putWord8    (addrSize (arpSPA msg))
    put         (arpOper msg)
    put         (arpSHA msg)
    put         (arpSPA msg)
    put         (arpTHA msg)
    put         (arpTPA msg)


-- Arp Opcodes -----------------------------------------------------------------

data ArpOper
  = ArpRequest -- ^ 0x1
  | ArpReply   -- ^ 0x2
  deriving (Eq)


instance Serialize ArpOper where
  get = do
    b <- getWord16be
    case b of
      0x1 -> return ArpRequest
      0x2 -> return ArpReply
      _   -> fail "invalid Arp opcode"

  put ArpRequest = putWord16be 0x1
  put ArpReply   = putWord16be 0x2
