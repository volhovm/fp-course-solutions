{-# LANGUAGE NoImplicitPrelude #-}

module BTree where

import           Data.Foldable    (foldr')
import qualified Data.List        as L (find)
import           Data.Tree        (Tree (..))
import           Data.Tree.Pretty (drawVerticalTree)
import           Prelude          hiding (elem, log)
import           System.Random    (randomRIO)


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
elem v (BNode a l r) =
    case compare a v of
        EQ -> True
        LT -> v `elem` l
        GT -> v `elem` r

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
