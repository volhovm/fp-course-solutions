{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Main (main) where

import           Control.Lens           (makeLenses, to, use, uses, (%=), (+~),
                                         (-=), (.=), (^.), _head)
import           Control.Monad          (join, replicateM, replicateM_, when)
import           Control.Monad.Extra    (unlessM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Loops    (untilM_, whileM_)
import           Control.Monad.State    (MonadState (..), execStateT)
import           Prelude                hiding (log)
import           System.Random          (StdGen, mkStdGen, randomR)


safeTail :: [a] -> Either String [a]
safeTail [] = Left "Couldn't get the tail of empty list"
safeTail x  = Right $ tail x

safeInit :: [a] -> Either String a
safeInit [] = Left "Can't get the head of empty list"
safeInit x  = Right $ head x

strip :: [a] -> Either String [a]
strip x = reverse <$> join (safeTail . reverse <$> safeTail x)

-----------------------------------------------

data Stats = Stats
    { _statHealth  :: Int
    , _statAttack  :: Int
    , _statDefense :: Int
    }

$(makeLenses ''Stats)

instance Show Stats where
    show Stats{..} = show [_statHealth, _statAttack, _statDefense]

isDead :: Stats -> Bool
isDead x = x ^. statHealth <= 0

data Loot = Loot
    { applyLoot :: Stats -> Stats
    , name      :: String
    }

healthPotion, attackPotion, defensePotion, штанизатридцятьгривень :: Loot
healthPotion = Loot (statHealth +~ 10) "health potion"
attackPotion = Loot (statAttack +~ 5) "attack potion"
defensePotion = Loot (statDefense +~ 5) "defense potion"
штанизатридцятьгривень = Loot (statDefense +~ 15) "ukrainian death shield"

allLoots :: [Loot]
allLoots = [healthPotion, attackPotion, defensePotion, штанизатридцятьгривень]

data GameState = GameState
    { _userStats :: Stats
    , _monsters  :: [Stats]
    , _stdgen    :: StdGen
    }

$(makeLenses ''GameState)

genInt :: (MonadState GameState m) => (Int, Int) -> m Int
genInt range = do
    (value,gen') <- uses stdgen $ randomR range
    stdgen .= gen'
    pure value

genLoot :: (MonadState GameState m) => m Loot
genLoot = (allLoots !!) <$> genInt (0, length allLoots - 1)

addMonster :: (MonadState GameState m) => m ()
addMonster = do
    [i,j,k] <- replicateM 3 $ genInt (40,60)
    monsters %= (Stats (i * 2) j (k `div` 2) :)

beginGloriousBattle :: (MonadState GameState m, MonadIO m) => m ()
beginGloriousBattle =
    whileM_ battlePredicate (fightTopMonster >> logRound)
  where
    log = liftIO . putStrLn
    logRound = do
        log "------- new round -------"
        reportUserStats
        l <- uses monsters length
        log $ show l ++ " monsters left"
    battlePredicate = do
         dead <- uses userStats isDead
         left <- uses monsters $ not . null
         when dead $ liftIO $ putStrLn
             "Our hero is dead, sorry for my shitty code that done this to him"
         when (not left) $ liftIO $
             putStrLn "All monsters are defeated, yay"
         pure (not dead && left)
    someoneIsDead = (||) <$> uses userStats isDead <*> uses (monsters . to head) isDead
    reportUserStats =
        use userStats >>= (\s -> log $ "User: " ++ show s)
    reportTopMonsterStats =
        use (monsters . to head) >>= (\s -> log $ "Monster: " ++ show s)
    giveSomeLoot = do
        Loot{..} <- genLoot
        log $ "User got loot '" ++ name ++ "'"
        userStats %= applyLoot
        reportUserStats
    fightTopMonster = do
        uAttack <- use $ userStats . statAttack
        uDefense <- use $ userStats . statDefense
        mAttack <- use $ monsters . to head . statAttack
        mDefense <- use $ monsters . to head .  statDefense
        let uEffAttack = max 5 $ uAttack - mDefense
            mEffAttack = max 5 $ mAttack - uDefense
        untilM_ (do log $ "Hero hits " ++ show uEffAttack
                    monsters . _head . statHealth -= uEffAttack
                    reportTopMonsterStats
                    unlessM (uses (monsters . to head) isDead) $ do
                        log $ "Monster hits " ++ show mEffAttack
                        userStats . statHealth -= mEffAttack
                        reportUserStats)
                someoneIsDead
        unlessM (uses userStats isDead) $ do
            giveSomeLoot
            log "monster defeated"
            monsters %= tail

startGame :: Int -> IO ()
startGame monstersN = do
    GameState{..} <- execStateT game $ GameState heroStats [] $ mkStdGen 5
    print _userStats
  where
    heroStats = Stats 100 50 50
    game = do
      replicateM_ monstersN addMonster
      beginGloriousBattle

main = undefined
