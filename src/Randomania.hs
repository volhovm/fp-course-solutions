{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

-- | HW 11 solutions

module Randomania where

import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar,
                                               swapTMVar, takeTMVar, tryReadTMVar,
                                               tryTakeTMVar)
import           Control.Concurrent.STM.TVar  (TVar, modifyTVar, newTVarIO, writeTVar)
import           Control.Lens                 (makeLenses, to, toListOf, traversed, use,
                                               uses, view, (%=), (^.))
import qualified Control.Monad.STM            as STM
import qualified Crypto.Sign.Ed25519          as Ed25519
import           Data.Binary                  (Binary)
import           Data.Default                 (def)
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as M
import           Data.Time.Clock              (UTCTime)
import           System.Random                (Random, StdGen, mkStdGen, newStdGen,
                                               randomR)
import           System.Wlog                  (logDebug, logError, logInfo)
import qualified System.Wlog                  as W
import           Universum

import           Crypto                       (PublicKey, SecretKey, Signature, Signed,
                                               checkSig, fromSigned, keyGen, mkSigned,
                                               sign, toPublic)


type BeingId = PublicKey
type BeingEssense = SecretKey

-- | Actions is rationalized matter interaction
data Action
    = Attack BeingId
      -- ^ Attacking another sentient being (why would you?)
    | GenRandom Int Int
    | Die
      -- ^ Sometimes it's better to go by yourself than experience the
      -- shame of defeat. This action is also executed after being
      -- worker is over.
    | AttainNirvana
      -- ^ Well, what happens when you don't want to play a game?
    deriving (Show, Generic)

instance Binary Action

data WorldResponse
    = Random Int
    | AttackPerformed
    | You'reDeadMan
    deriving (Show, Generic)

instance Binary WorldResponse

-- | What a sentient being really is as a physical body.
data Being = Being
    { _bId          :: BeingId
      -- ^ Your physical parameters define how people distinguish you from else.
    , _bHealth      :: Int
      -- ^ Physical body parameters. Negative == dead.
    , _bSpeed       :: Int
      -- ^ Attack speed
    , _bInstability :: Int
      -- ^ Magic instability
    , _bGenerators  :: NonEmpty StdGen
      -- ^ Random sources
    , _bKarma       :: [(Action,UTCTime)]
      -- ^ Actions considered to be successfull from dharma's POV
      -- (e.g. user requested to wait and we sent him a confirmation
      -- later).
    }

makeLenses ''Being

instance Eq Being where
    (==) a b = a ^. bId == b ^. bId
instance Ord Being where
    compare = comparing (view bId)

-- | What a sentient being thinks he is.
data BeingReflection = BeingReflection
    { _reflEssense     :: BeingEssense
      -- ^ Your body is a proof that people recognize you as you.
    , _reflAction      :: TMVar (Signed Action)
      -- ^ Action is what you think will happen next. Put action here to act.
    , _reflResponse    :: TMVar (Signed WorldResponse)
      -- ^ But dharma decides how it will unfold.
    , _reflInspect     :: TVar (Being,[Being])
      -- ^ You can also reflect on some worldview, but changing this
      -- variable wouldn't cause any consequences. It's re-filled
      -- write-only by dharma.
    , _reflDharmaTrust :: PublicKey
      -- ^ Used to check dharma replies are _real_ (haha). It's
      -- basically introspection & memory -- you can go solipsistic
      -- and say nothing except for reflId/reflEssense is true.
    }

makeLenses ''BeingReflection

instance Eq BeingReflection where
    (==) a b = a ^. reflEssense == b ^. reflEssense
instance Ord BeingReflection where
    compare = comparing (view reflEssense)

-- | The great secret state of dharma (naturalized)
data DharmaState = DharmaState
    { _creatures :: Map BeingReflection Being
      -- ^ Log of all creatures that exist
    , _dharmaSk  :: SecretKey
      -- ^ Way to show something happens.
    }

makeLenses ''DharmaState

-- | Magician is sentient being that can modify its own
-- reflection. Chaniging any parameters of his own reflection is
-- possible, but meaningless in terms of dharma.
newtype MagicianM a = MagicianM
    { getMagicianM :: StateT BeingReflection IO a
    } deriving (Functor, Applicative, Monad, MonadState BeingReflection, MonadIO, W.CanLog)

instance W.HasLoggerName MagicianM where
    getLoggerName = do
        id <- uses reflEssense toPublic
        pure $ W.LoggerName $ "Magician#" <> take 6 (show id)
    modifyLoggerName _ = identity

newtype DharmaM a = DharmaM
    { fromDharmaM :: StateT (DharmaState) IO a
    } deriving (Functor, Applicative, Monad, MonadState DharmaState, MonadIO, W.CanLog)

instance W.HasLoggerName DharmaM where
    getLoggerName = pure $ W.LoggerName $ "Dharma"
    modifyLoggerName _ = identity

commitAction :: Action -> MagicianM ()
commitAction action = do
    sk <- use reflEssense
    actVar <- use reflAction
    atomically $ putTMVar actVar $ mkSigned sk action

waitResponse :: MagicianM WorldResponse
waitResponse = do
    respVar <- use reflResponse
    resp <- atomically $ takeTMVar respVar
    respM <- uses reflDharmaTrust $ \pk -> fromSigned pk resp
    maybe onFailure pure respM
  where
    onFailure = do
        logError "I don't believe this world anymore"
        pure You'reDeadMan

-- | A regular suffering magician that fights for nothing.
magicianSuffer :: MagicianM ()
magicianSuffer = do
    logInfo $ "Waiting for the world to tell me what to do"
    resp <- waitResponse
    logInfo $ "The world told me: " <> show resp

kamikaze :: MagicianM ()
kamikaze = commitAction Die

toListOfM l k = uses l $ toListOf k

spawnCreatures :: [MagicianM ()] -> DharmaM ()
spawnCreatures magicians = do
    dharmaPk <- uses dharmaSk toPublic
    params <- replicateM (length magicians) (genParams dharmaPk)
    let beings = map fst params
    forM_ params $ \(being,refl) -> do
        liftIO $ atomically $
            modifyTVar (refl ^. reflInspect) $
            second $ const $ filter (/= being) beings
        creatures %= M.insert refl being
    forM_ (params `zip` magicians) $ \((_,refl),magician) -> do
        liftIO $ threadDelay $ 5 * 100 * 1000
        putText "Starting magician thread"
        void $ liftIO $ forkIO $ void $ do
            runStateT (getMagicianM (magician >> commitAction Die)) refl
  where
    genParams :: PublicKey -> DharmaM (Being, BeingReflection)
    genParams dharmaPk = do
        stdgen <- liftIO newStdGen
        (pk,sk) <- keyGen
        let (defense, g') = randomR (1,200) stdgen
            (speed, g'') = randomR (1,20) g'
            (instability, g''') = randomR (10^7, 2 * 10^7) g''
            being = Being pk defense speed instability (g''' :| []) []
        req <- liftIO newEmptyTMVarIO
        resp <- liftIO newEmptyTMVarIO
        reader <- liftIO $ newTVarIO (being,[])
        let bRefl = BeingReflection sk req resp reader dharmaPk
        pure (being, bRefl)

-- | Applies user action and returns a response plus new being state
-- (in case being died and is not in creatures list anymore).
applyAction :: Maybe Action -> DharmaM (WorldResponse, Being)
applyAction = undefined

-- | Dharma we're used to -- no sudden/unexpected actions happen.
spinSansara :: DharmaM ()
spinSansara = do
    refls <- toListOfM creatures $ to M.assocs . traversed
    sk <- use dharmaSk
    -- Hi there! You're dead!
    logInfo "Dharma reporting: killing everyone"
    rawActions <-
        atomically $ forM refls $
        \(refl,being) -> fmap (refl,being,) <$> tryTakeTMVar (refl ^. reflAction)
--        void $ putTMVar (refl ^. reflResponse) $ mkSigned sk You'reDeadMan
    let First actionM = mconcat $ map First rawActions
    whenJust actionM $ \(refl,being,action) -> do
        let checkedAction = fromSigned (being ^. bId) action
        (resp,newBeing) <- applyAction checkedAction
        signedResp <- uses dharmaSk $ \sk -> mkSigned sk resp
        allBeings <- uses creatures M.elems
        atomically $ do
            swapTMVar (refl ^. reflResponse) signedResp
            writeTVar (refl ^. reflInspect) (newBeing,filter (/=newBeing) allBeings)
    logInfo "Sansara wheel just made another spin"
    liftIO $ threadDelay $ 1 * 1000 * 1000

rollSansara :: IO ()
rollSansara = do
    W.initLoggingWith def W.Info
    (_, dharmaSk) <- keyGen
    void $ runStateT (fromDharmaM bootstrap)
        (DharmaState M.empty dharmaSk)
  where
    bootstrap = do
        logInfo "Starting the univere, wzooh"
        spawnCreatures $ replicate 1 magicianSuffer
        forever spinSansara
