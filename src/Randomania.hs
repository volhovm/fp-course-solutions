{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

-- | HW 11 solutions

module Randomania where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM.TMVar (TMVar)
import           Control.Concurrent.STM.TVar  (TVar)
import           Control.Monad.STM            (atomically)
import qualified Crypto.Sign.Ed25519          as Ed25519
import           Data.Binary                  (Binary)
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import qualified Data.List.NonEmpty           as NE
import           System.Random                (Random, StdGen, mkStdGen, randomR)
import           Universum

import           Crypto                       (PublicKey, SecretKey, Signature)

type BeingId = Ed25519.PublicKey
type BeingEssense = Ed25519.SecretKey

-- | Actions is rationalized matter interaction
data Action
    = Attack Int BeingId
    | Wait Int
    deriving (Generic)
instance Binary Action

type ActionProof = (Action, Signature Action)

data WorldResponse
    = Signature Action
    | You'reDeadMan
    deriving (Generic)
instance Binary WorldResponse

-- | What a sentient being really is as a physical body.
data Being = Being
    { _pHealth      :: Int
      -- ^ Physical body parameters. Negative == dead.
    , _pSpeed       :: Int
      -- ^ Attack speed
    , _pInstability :: Int
      -- ^ Magic instability
    , _pGenerators  :: NonEmpty StdGen
      -- ^ Random sources
    }

-- | What a sentient being thinks he is.
data BeingReflection = BeingReflection
    { _reflId       :: BeingId
      -- ^ Your physical parameters define how people distinguish you from else.
    , _reflEssense  :: BeingEssense
      -- ^ Your body is a proof that people recognize you as you.
    , _reflAction   :: TMVar Action
      -- ^ Action is what you think will happen next.
    , _reflResponse :: TMVar WorldResponse
      -- ^ But dharma decides how it will unfold.
    , _reflInspect  :: MVar Being
      -- ^ You can also reflect on your body, but changing this variable
      -- doesn't makes sense. It's re-filled by dharma.
    }

-- | This datatype contains secret state formed by being's actions.
data BeingKarma = BeingKarma
    { _kActions :: [Action]
      -- ^ Actions considered to be successfull from dharma's POV
      -- (e.g. user requested to wait and we sent him a confirmation
      -- later).
    }

-- | The great secret state of dharma (naturalized)
data DharmaState = DharmaState
    { _karma     :: Map Being BeingKarma
      -- ^ Beings' karma
    , _creatures :: Map Being BeingReflection
      -- ^ Log of all creatures that exist
    }

-- | Magician is sentient being that can modify its own
-- reflection. Chaniging any parameters of his own reflection is
-- possible, but meaningless in terms of dharma.
newtype MagicianM a = MagicianM
    { getMagicianM :: StateT BeingReflection IO a
    } deriving (Functor, Applicative, Monad, MonadState BeingReflection, MonadIO)

newtype DharmaM a = DharmaM
    { fromDharmaM :: StateT (DharmaState) IO a
    } deriving (Functor, Applicative, Monad, MonadState DharmaState, MonadIO)


spawnCreature :: MagicianM () -> DharmaM ()
spawnCreature magician = do
    let being = Being 200 10 (10 ^ 7) (mkStdGen 12345 :| [])
        bRefl :: BeingReflection
        bRefl = undefined
    liftIO $ forkIO $ void $ runStateT (getMagicianM magician) bRefl
    undefined

sansaraRound :: DharmaM ()
sansaraRound = do
    undefined

kek :: Text
kek = "aoeu"
