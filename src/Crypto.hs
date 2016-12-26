{-# LANGUAGE DeriveGeneric #-}
-- | Signing primitives (copied from cardano-sl)

module Crypto
       (
       -- * Keys
         PublicKey (..)
       , SecretKey (..)
       , keyGen
       , deterministicKeyGen
       , toPublic

       -- * Signing and verification
       , Signature (..)
       , sign
       , checkSig

       , Signed (..)
       , mkSigned
       ) where

import           Control.Monad.Fail     (fail)
import qualified Crypto.Sign.Ed25519    as Ed25519
import           Data.Binary            (Binary)
import qualified Data.Binary            as Binary
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Coerce            (coerce)
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, later, (%))
import           OpenSSL.Random         (randBytes)
import           Universum

instance Binary Ed25519.SecretKey
instance Binary Ed25519.PublicKey

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.PublicKey'.
newtype PublicKey = PublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Generic)

-- | Wrapper around 'Ed25519.SecretKey'.
newtype SecretKey = SecretKey Ed25519.SecretKey
    deriving (Eq, Ord, Show, Generic)

-- | Generate a public key from a secret key. Fast (it just drops some bytes
-- off the secret key).
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey k) = PublicKey (Ed25519.secretToPublicKey k)

-- | Generate a key pair.
keyGen :: MonadIO m => m (PublicKey, SecretKey)
keyGen = liftIO $ do
    seed <- liftIO $ randBytes 32
    case Ed25519.createKeypairFromSeed_ seed of
        Nothing -> panic "Pos.Crypto.Signing.keyGen:\
                         \ createKeypairFromSeed_ failed"
        Just (pk, sk) -> return (PublicKey pk, SecretKey sk)

-- | Create key pair deterministically from 32 bytes.
deterministicKeyGen :: BS.ByteString -> Maybe (PublicKey, SecretKey)
deterministicKeyGen seed =
    bimap PublicKey SecretKey <$> Ed25519.createKeypairFromSeed_ seed

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.Signature'.
newtype Signature a = Signature Ed25519.Signature
    deriving (Eq, Ord, Show, Generic)

-- | Encode something with 'Binary' and sign it.
sign :: Binary a => SecretKey -> a -> Signature a
sign k = coerce . signRaw k . BSL.toStrict . Binary.encode
  where
    signRaw :: SecretKey -> ByteString -> Signature ByteString
    signRaw (SecretKey k) x = Signature (Ed25519.dsign k x)

-- CHECK: @checkSig
-- | Verify a signature.
-- #verifyRaw
checkSig :: Binary a => PublicKey -> a -> Signature a -> Bool
checkSig k x s = verifyRaw k (BSL.toStrict (Binary.encode x)) (coerce s)
  where
    verifyRaw :: PublicKey -> ByteString -> Signature ByteString -> Bool
    verifyRaw (PublicKey k) x (Signature s) = Ed25519.dverify k x s

-- | Value and signature for this value.
data Signed a = Signed
    { signedValue :: !a              -- ^ Value to be signed
    , signedSig   :: !(Signature a)  -- ^ 'Signature' of 'signedValue'
    } deriving (Show, Eq, Ord, Generic)

-- | Smart constructor for 'Signed' data type with proper signing.
mkSigned :: (Binary a) => SecretKey -> a -> Signed a
mkSigned sk x = Signed x (sign sk x)
