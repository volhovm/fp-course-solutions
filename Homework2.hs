{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main (main) where

import           Control.Lens           (makeLenses, to, use, uses, (%=), (+~), (-=),
                                         (.=), (^.), _head)
import           Control.Monad          (join, replicateM, replicateM_, when)
import           Control.Monad.Extra    (unlessM)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Loops    (untilM_, whileM_)
import           Control.Monad.State    (MonadState (..), execStateT)
import           Data.Foldable          (foldr')
import qualified Data.List              as L (find)
import qualified Data.Set               as S
import           Data.Tree              (Tree (..))
import           Data.Tree.Pretty       (drawVerticalTree)
import           Debug.Trace
import           Prelude                hiding (delete, elem, log)
import           System.Random          (StdGen, mkStdGen, randomR, randomRIO)


safeTail :: [a] -> Either String [a]
safeTail [] = Left "Couldn't get the tail of empty list"
safeTail x  = Right $ tail x

safeInit :: [a] -> Either String [a]
safeInit [] = Left "Can't get the init of empty list"
safeInit x  = Right $ init x

strip :: [a] -> Either String [a]
strip x = join $ safeInit <$> safeTail x

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


-----------------------------------------------

data BTree a = BNode a (BTree a) (BTree a) | BNil
               deriving (Show)

toTree :: (Show a) => BTree a -> Tree String
toTree (BNode e BNil BNil ) = Node (show e) []
toTree (BNode e BNil b)     = Node (show e) [toTree b]
toTree (BNode e a BNil )    = Node (show e) [toTree a]
toTree (BNode e a b)        = Node (show e) [toTree a, toTree b]
toTree BNil                 = Node "*" []

printTree :: (Show a) => BTree a -> IO ()
printTree = putStrLn . drawVerticalTree . toTree

-- malformed tho
fullTree :: Int -> IO (BTree Int)
fullTree a | a < 0 = error "positive depth expected"
fullTree 0 = pure BNil
fullTree n = BNode <$> randomRIO (0, 99) <*> fullTree (n-1) <*> fullTree (n-1)

-- Doesn't work with infinite lists
toList :: BTree a -> [a]
toList k = toList' k []
  where
    toList' :: BTree a -> ([a] -> [a])
    toList' (BNode a l r) = toList' l . (a :) . toList' r
    toList' BNil          = id

toListPlain :: BTree a -> [a]
toListPlain (BNode a l r) = toListPlain l ++ [a] ++ toListPlain r
toListPlain BNil          = []

{- Diff lists!

λ> tree <- fullTree 20
(6.67 secs, 2,499,906,896 bytes)
λ> let listPlain = toListPlain tree
(0.00 secs, 70,880 bytes)
λ> let listDiff = toList tree
0.00 secs, 70,800 bytes)
λ> show $ last listPlain
"35"
(2.05 secs, 1,585,555,440 bytes)
λ> show $ last listDiff
"35"
(1.08 secs, 536,982,352 bytes)

-}

toListInf :: BTree a -> [a]
toListInf t             = bfs [t]
  where
    bfs []             = []
    bfs (x:xs) = case x of
        BNode a l r -> a : (bfs $ l : r : xs)
        BNil        -> bfs xs

-- Works with infinite lists -- hangs
fromList :: (Ord a) => [a] -> BTree a
fromList [] = BNil
fromList xs = foldr' insert BNil xs

-- Shouldn't work with infinite lists
find :: (a -> Bool) -> BTree a -> Maybe a
find predic = L.find predic . toList

-- Works with infinite trees
elem :: (Ord a) => a -> BTree a -> Bool
elem _ BNil          = False
elem v (BNode a l r) = a == v || v `elem` l || v `elem` r

-- works with infinite trees
insert :: (Ord a) => a -> BTree a -> BTree a
insert v BNil          = BNode v BNil BNil
insert v a@(BNode e l r)
    | v == e = a
    | v < e = BNode e (insert v l) r
    | v > e = BNode e l (insert v r)

delete :: (Ord a) => a -> BTree a -> BTree a
delete v (BNode e l r) | e == v =
    case (l,r) of
        (BNil, BNil)             -> BNil
        (BNil, n@BNode{})        -> n
        (n@BNode{}, BNil)        -> n
        (nl, nr) -> let e' = leftmost nr
                    in BNode e' nl (delete e' nr)
delete v (BNode e l r) | v < e = BNode e (delete v l) r
delete v (BNode e l r) | v > e = BNode e l (delete v r)
delete _ BNil = error "delete shouldn't reach bnil"

leftmost :: BTree a -> a
leftmost BNil             = error "shouldn't be called of nil"
leftmost (BNode e BNil _) = e
leftmost (BNode _ l _)    = leftmost l

-- stupid delete implementation that works with infinite lists (hehe)
deleteInf :: (Ord a) => a -> BTree a -> BTree a
deleteInf v (BNode e l r) | e == v =
    case (l,r) of
        (BNil, BNil)             -> BNil
        (BNil, n@BNode{})        -> n
        (n@BNode{}, BNil)        -> n
        (nl, nr) -> let flattenLists x y = concatMap (\(a,b) -> [a,b]) $ zip x y
                        in fromList $ flattenLists (toListInf nl) (toListInf nr)

deleteInf v (BNode e l r) | v < e = BNode e (delete v l) r
deleteInf v (BNode e l r) | v > e = BNode e l (delete v r)
deleteInf _ BNil          = error "delete shouldn't reach bnil"

----------------------------------------
-- binomial heap --

main = undefined
