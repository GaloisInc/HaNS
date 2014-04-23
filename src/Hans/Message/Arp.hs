module Hans.Message.Arp where

import Hans.Address (Address(addrSize))
import Hans.Utils (chunk)

import Control.Applicative (Applicative(..),(<$>))
import Data.Serialize.Get (Get,runGet,getWord8,getWord16be)
import Data.Serialize.Put (Putter,runPut,putWord16be,putWord8)
import Data.Word (Word16)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L


-- Arp Packets -----------------------------------------------------------------

data ArpPacket hw p = ArpPacket
  { arpHwType :: !Word16
  , arpPType  :: !Word16
  , arpOper   :: ArpOper
  , arpSHA    :: hw
  , arpSPA    :: p
  , arpTHA    :: hw
  , arpTPA    :: p
  } deriving (Show)

-- | Parse an Arp packet, given a way to parse hardware and protocol addresses.
parseArpPacket :: Get hw -> Get p
               -> S.ByteString -> Either String (ArpPacket hw p)
parseArpPacket getHw getP = runGet $
      ArpPacket
  <$> getWord16be  -- hardware type
  <*> getWord16be  -- protocol type
  <*  getWord8     -- hardware address length (ignored)
  <*  getWord8     -- protocol address length (ignored)
  <*> parseArpOper -- operation
  <*> getHw        -- sender hardware address
  <*> getP         -- sender protocol address
  <*> getHw        -- target hardware address
  <*> getP         -- target protocol address

-- | Render an Arp packet, given a way to render hardware and protocol
-- addresses.
renderArpPacket :: (Address hw, Address p)
                => Putter hw -> Putter p
                -> ArpPacket hw p -> L.ByteString
renderArpPacket putHw putP arp = chunk $ runPut $ do
  putWord16be   (arpHwType arp)
  putWord16be   (arpPType arp)
  putWord8      (addrSize (arpSHA arp))
  putWord8      (addrSize (arpSPA arp))
  renderArpOper (arpOper arp)
  putHw         (arpSHA arp)
  putP          (arpSPA arp)
  putHw         (arpTHA arp)
  putP          (arpTPA arp)


-- Arp Opcodes -----------------------------------------------------------------

-- | Arp operations.
data ArpOper
  = ArpRequest -- ^ 0x1
  | ArpReply   -- ^ 0x2
  deriving (Show,Eq)

-- | Parse an Arp operation.
parseArpOper :: Get ArpOper
parseArpOper  = do
  b <- getWord16be
  case b of
    0x1 -> return ArpRequest
    0x2 -> return ArpReply
    _   -> fail "invalid Arp opcode"

-- | Render an Arp operation.
renderArpOper :: Putter ArpOper
renderArpOper op = case op of
  ArpRequest -> putWord16be 0x1
  ArpReply   -> putWord16be 0x2
