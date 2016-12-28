{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

-- | HW 11 solutions

module Randomania (rollSansara) where

import qualified Base                         as Base
import           Control.Concurrent           (forkIO)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar,
                                               swapTMVar, takeTMVar, tryReadTMVar,
                                               tryTakeTMVar)
import           Control.Concurrent.STM.TVar  (TVar, modifyTVar, newTVarIO, readTVar,
                                               writeTVar)
import           Control.Lens                 (at, filtered, ix, makeLenses, to, toListOf,
                                               traversed, use, uses, view, (%=), (%~),
                                               (-~), (.=), (.~), (<%=), (?=), (?~), (^.),
                                               (^?), _2)
import qualified Control.Monad.STM            as STM
import           Control.Monad.Trans.Maybe    (MaybeT (..), runMaybeT)
import qualified Crypto.Sign.Ed25519          as Ed25519
import           Data.Binary                  (Binary)
import           Data.Default                 (Default (def))
import           Data.List                    (findIndex)
import           Data.List.NonEmpty           (NonEmpty ((:|)))
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map                     as M
import qualified Data.Semigroup               as S
import           Data.Time.Clock              (UTCTime, addUTCTime, getCurrentTime)
import           System.Random                (Random, StdGen, mkStdGen, newStdGen,
                                               randomR)
import           System.Wlog                  (LoggingFormat (..), logDebug, logError,
                                               logInfo, logNotice)

import qualified System.Wlog                  as W
import           Universum

import           Crypto                       (PublicKey, SecretKey, Signature, Signed,
                                               checkSig, fromSigned, keyGen, mkSigned,
                                               sign, toPublic)

----------------------------------------------------------------------------
-- Datatypes
----------------------------------------------------------------------------

type BeingId = PublicKey
type BeingEssense = SecretKey
type Delay = Int

-- | Actions is rationalized matter interaction
data Action
    = ChooseCreature
      -- ^ You ask your random karmic generator to point you at somebody.
    | CastAttack
      -- ^ You decide to start attack -- first need to generate params.
    | PerformAttack
      -- ^ Execute attack on another sentient being (why would you?).
    | Die
      -- ^ Sometimes it's better to go by yourself than experience the
      -- shame of defeat. This action is also executed after being
      -- worker is over.
    | AttainBodhi
      -- ^ Well, what happens when you don't want to play a game?
    deriving (Show, Generic)

instance Binary Action

-- | How being rationalize their sensory input
data WorldResponse
    = WBeing BeingId
      -- ^ Some random being was returned.
    | WCastedAttack Int Delay
      -- ^ You've received a chance to perform attack with <dmg,delay>.
    | WNope
      -- ^ Nothing was done.
    | WDone
      -- ^ Command was executed successfully.
    | You'reDeadMan
    deriving (Show, Generic)

instance Binary WorldResponse

-- | What a sentient being really is as a physical body.
data Being = Being
    { _bId          :: BeingId
      -- ^ Your physical parameters define how people distinguish you from else.
    , _bHealth      :: Int
      -- ^ Physical body parameters. Negative == dead.
    , _bMaxSpeed    :: Int
      -- ^ Maximum attack speed.
    , _bMaxAttack   :: Int
      -- ^ Maximum attack damage.
    , _bInstability :: Int
      -- ^ Magic instability
    , _bGenerators  :: NonEmpty StdGen
      -- ^ Random sources
    } deriving (Generic)

instance Base.Show Being where
    show Being {..} =
        concat [ "Being { bId = "
               , show _bId
               , ", bHealth = "
               , show _bHealth
               , ", bMaxSpeed = "
               , show _bMaxSpeed
               , ", bMaxAttack = "
               , show _bMaxAttack
               , ", bInstability = "
               , show _bInstability
               , ", bGenerators = <"
               , show (NE.length _bGenerators)
               , "> }"
               ]

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

-- | Secret information dharma knows about being.
data Karma = Karma
    { _kTargetRequest :: Maybe BeingId
    , _kAttackRequest :: Maybe (Int, Delay, UTCTime)
    } deriving (Show)

-- | Empty as a clear paper sheet.
instance Default Karma where def = Karma Nothing Nothing

makeLenses ''Karma

-- | The great secret state of dharma (naturalized)
data DharmaState = DharmaState
    { _creatures :: Map BeingReflection Being
      -- ^ Log of all creatures that exist
    , _karma     :: Map BeingReflection Karma
      -- ^ Karmic log
    , _dharmaSk  :: SecretKey
      -- ^ Way to show something happens.
    }

makeLenses ''DharmaState

----------------------------------------------------------------------------
-- Monads definitions
----------------------------------------------------------------------------

-- | Magician is sentient being that can modify its own
-- reflection. Chaniging any parameters of his own reflection is
-- possible, but meaningless in terms of dharma.
newtype MagicianM a = MagicianM
    { getMagicianM :: StateT BeingReflection IO a
    } deriving (Functor, Applicative, Monad, MonadState BeingReflection, MonadIO, W.CanLog,MonadThrow, MonadCatch)

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

----------------------------------------------------------------------------
-- Sansara logic
----------------------------------------------------------------------------

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
        karma %= M.insert refl def
    forM_ (params `zip` magicians) $ \((being,refl),magician) -> do
        liftIO $ threadDelay $ 5 * 100 * 1000
        logInfo $ "Starting magician thread for " <> show (being ^. bId)
        void $ liftIO $ forkIO $ void $ do
            let runCycle = do
                    magician `catch` (\(e::SomeException) -> pass)
                    commitAction Die
            runStateT (getMagicianM runCycle) refl
            putText $ "Thread of magician " <> show (being ^. bId) <> " was terminated"
  where
    genParams :: PublicKey -> DharmaM (Being, BeingReflection)
    genParams dharmaPk = do
        stdgen <- liftIO newStdGen
        (pk,sk) <- keyGen
        let (defense, g1) = randomR (1,200) stdgen
            (speed, g2) = randomR (1,20) g1
            (maxAttack, g3) = randomR (1,100) g2
            (instability, g4) = randomR (10^7, 2 * 10^7) g3
            being = Being pk defense speed maxAttack instability (g4 :| [])
        req <- liftIO newEmptyTMVarIO
        resp <- liftIO newEmptyTMVarIO
        reader <- liftIO $ newTVarIO (being,[])
        let bRefl = BeingReflection sk req resp reader dharmaPk
        pure (being, bRefl)

-- | Applies user action and returns a response plus new being state
-- (in case being died and is not in creatures list anymore).
applyAction :: BeingReflection
            -> Maybe Action
            -> DharmaM (Maybe (WorldResponse, Being))
applyAction refl Nothing = fmap (WNope, ) <$> uses creatures (M.lookup refl)
applyAction refl (Just action) = runMaybeT $ do
    being <- MaybeT (uses creatures $ M.lookup refl)
    lift $ case action of
        ChooseCreature    -> do
            (otherBeings :: [(BeingReflection,Being)]) <- toListOfM creatures $
                to M.assocs . traversed . filtered ((/= refl) . fst)
            let (g:|gs) = being ^. bGenerators
                (chosenN,g') = randomR (0,pred $ length otherBeings) g
                b' = being & bGenerators .~ (g':|gs)
                chosenBeing = otherBeings ^? ix chosenN
                decision = maybe WNope (WBeing . (view $ _2 . bId)) chosenBeing
            creatures . at refl ?= b'
            whenJust chosenBeing $ \b -> do
                karma . at refl %= fmap (kTargetRequest ?~ (b ^. _2 . bId))
                karma . at refl %= fmap (kAttackRequest .~ Nothing)
            logDebug $ "Responding with: " <> show decision
            (decision,) <$> uses creatures (lookup' refl)
        CastAttack        -> do
            let (g:|gs) = being ^. bGenerators
                (delay,g') = randomR (1, being ^. bMaxSpeed) g
                attacks = map (randomR (1, being ^. bMaxAttack)) (g':|gs)
                attack = maximum $ map fst attacks
                newGs = map snd attacks
            creatures . at refl %= fmap (bGenerators .~ newGs)
            castTime <- liftIO getCurrentTime
            karma . at refl %= fmap (kAttackRequest ?~ (attack, delay, castTime))
            (WCastedAttack attack delay,) <$> uses creatures (lookup' refl)
        PerformAttack -> do
            attackTime <- liftIO getCurrentTime
            res <- runMaybeT $ do
                karma <- MaybeT $ use $ karma . at refl
                target <- MaybeT $ pure $ view kTargetRequest karma
                (attack,delay,castTime) <- MaybeT $ pure $ view kAttackRequest karma
                guard $ fromIntegral delay `addUTCTime` castTime < attackTime
                targetRefl <- MaybeT $ uses creatures $
                    fmap fst . find ((== target) . view bId . snd) . M.assocs
                (newHealth :: Int) <- fmap (view bHealth) . MaybeT $
                    creatures . at targetRefl <%= fmap (bHealth -~ attack)
                when (newHealth <= 0) $ do
                    lift $ logNotice $
                        "Woah! Wizard " <> show (being ^. bId) <>
                        " has just killed " <>
                        show (targetRefl ^. reflEssense . to toPublic)
                    targetGens <- view bGenerators <$> MaybeT (use $ creatures . at targetRefl)
                    creatures . at refl %= fmap (bGenerators %~ (S.<> targetGens))
                    creatures %= M.delete targetRefl
            pure $ (maybe WNope (const WDone) res, being)
        Die               -> do
            isAlive <- uses creatures $ elem refl . M.keys
            -- What a banality, duh
            when isAlive $ do
                creatures . at refl %= fmap (bHealth .~ 0)
                creatures %= M.delete refl
            pure (bool WNope WDone isAlive, being)
        AttainBodhi       -> do
            -- Your karma is clear now
            karma . at refl ?= def
            -- Your health is full again
            bodhiBeing <- creatures . at refl <%= fmap (bHealth .~ 1000)
            -- The suffering is alleviated
            creatures %= M.delete refl
            pure (WDone, fromMaybe being bodhiBeing)
  where
    lookup' = M.findWithDefault (panic "applyAction@lookup'")
    undead = panic ("Being is undead! " <> show (refl ^. reflEssense . to toPublic))

-- | Dharma we're used to -- no sudden/unexpected actions happen.
spinSansara :: DharmaM ()
spinSansara = do
    refls <- toListOfM creatures $ to M.assocs . traversed
    sk <- use dharmaSk
    -- Collect all actions of this spin
    rawActions <-
        atomically $ forM refls $
        \(refl,being) -> fmap (refl,being,) <$> tryTakeTMVar (refl ^. reflAction)
    -- Process each action
    forM_ (catMaybes rawActions) $ \(refl,being,action) -> do
        -- Process action
        logDebug "Processing actions"
        let checkedAction = fromSigned (being ^. bId) action
        respM <- applyAction refl checkedAction
        logDebug "Applied action"
        let (resp,newBeing) = fromMaybe (You'reDeadMan,being) respM
        signedResp <- uses dharmaSk $ \sk -> mkSigned sk resp
        -- Retrieve current state
        aliveAll <- uses creatures M.assocs
        let aliveBeings = map snd aliveAll
        logDebug "Commiting changes"
        atomically $ do
            let overrideTMVar v a = tryTakeTMVar v >> putTMVar v a
            -- Respond to the current hero and update his resp/view
            overrideTMVar (refl ^. reflResponse) signedResp
            writeTVar (refl ^. reflInspect) (newBeing,filter (/= newBeing) aliveBeings)
            -- Also update everyone else's view
            forM_ (filter ((/= refl) . fst) aliveAll) $ \(refl,being) ->
                writeTVar (refl ^. reflInspect) (being,filter (/= being) aliveBeings)
        logDebug "This round changes commited"
    beingsLeft <- use $ creatures . to M.elems
    let beingsN = length beingsLeft
    if beingsN <= 1
    then do
        liftIO $ threadDelay $ 2 * 1000 * 1000
        logInfo $ "The game is over. Participants left: " <> show beingsN
        when (beingsN == 1) $ logInfo $
            "Here's a winner: " <> show (view bId <$> head beingsLeft)
    else spinSansara

----------------------------------------------------------------------------
-- Creatures logic
----------------------------------------------------------------------------

commitAction :: Action -> MagicianM ()
commitAction action = do
    sk <- use reflEssense
    actVar <- use reflAction
    atomically $ putTMVar actVar $ mkSigned sk action

waitResponse :: MagicianM WorldResponse
waitResponse = do
    respVar <- use reflResponse
    respM <- atomically $ tryTakeTMVar respVar
    maybe (liftIO (threadDelay pollDelay) >> waitResponse) onResp respM
  where
    pollDelay = 100 * 1000
    onResp resp = do
        respM <- uses reflDharmaTrust $ \pk -> fromSigned pk resp
        maybe onFailure pure respM
    onFailure = do
        logError "I don't believe this world anymore"
        pure You'reDeadMan

-- | A regular suffering magician that fights for nothing.
magicianSuffer :: MagicianM ()
magicianSuffer = do
    logDebug $ "Asking the world to choose a target"
    commitAction ChooseCreature
    logDebug $ "Waiting for the response"
    WBeing beingId <- waitResponse
    logInfo $ "I'll be attacking " <> show beingId
    commitAction CastAttack
    WCastedAttack damage delay <- waitResponse
    logInfo $ "I'll do " <> show damage <> " damage after " <> show delay <> " seconds"
    liftIO $ threadDelay $ delay * (1000 * 1005)
    logInfo "Attacking!"
    commitAction PerformAttack
    _ <- waitResponse
    -- suffering never ends
    var <- use reflInspect
    health <- view bHealth . fst <$> atomically (readTVar var)
    if | health < 40 -> do
             logNotice $
                 "I'm almost down, but will strike back! Stil have " <> show health <> " hp!"
             magicianSuffer
       | health <= 0 -> logNotice "I have no regrets, death!"
       | otherwise -> magicianSuffer

observer :: MagicianM ()
observer = do
    body <- getBody
    observerDo body
  where
    getBody = do
        var <- use reflInspect
        fst <$> atomically (readTVar var)
    observerDo body = do
        newBody <- getBody
        let newHealth = newBody ^. bHealth
        when (newHealth /= body ^. bHealth) $ do
            if | newHealth <= 0 -> logNotice "Embrace me, death."
               | newHealth < 40 ->
                 logNotice $ "My health is critical: " <> show newHealth <>
                              ". I'm ready to meet the death."
               | otherwise ->
                 logInfo $ "I'm slowly dying with health " <> show newHealth <>
                           ". There's nothing i can do about it."
        when (newHealth > 0) $ observerDo newBody

rollSansara :: IO ()
rollSansara = do
    W.initLoggingWith (LoggingFormat False) W.Info
    (_, dharmaSk) <- keyGen
    void $ runStateT (fromDharmaM bootstrap)
        (DharmaState M.empty M.empty dharmaSk)
  where
    bootstrap = do
        logInfo "Starting the universe, wzooh"
        -- Scenario: magician fight
        spawnCreatures $ observer : replicate 5 magicianSuffer
        -- Scenario: massacre in the monastery
        --spawnCreatures $ magicianSuffer : replicate 10 observer
        spinSansara
