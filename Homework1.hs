module Main (main) where

import           Control.Lens
import           Control.Monad  (forM_, unless)
import           Data.Bifunctor (bimap, first)
import           Data.Char      (isDigit, isSpace)
import           Data.Maybe     (catMaybes, isJust)
import           System.Random  (newStdGen, randomRs)

stringSum :: String -> Int
stringSum "" = 0
stringSum s = loop 0 s
  where
    fail' = error "can't parse"
    parseInt :: String -> (Int,String)
    parseInt s' = case reads s' of
         []    -> fail'
         (x:_) -> x
    loop acc s' =
        let cont = uncurry loop . first (+acc) . parseInt in
        case dropWhile isSpace s' of
            []       -> acc
            ('+':xs) -> if isDigit (head xs) then cont xs else fail'
            s''      -> cont s''

{-
λ> zipN sum [[1], [], [3]]
[1]
λ> zipN sum [ [1, 2, 3]  , [4, 5, 6] , [7, 8, 9]]
[12,15,18]
λ> take 5 $ zipN (take 3) $ repeat [0..]
[[0,0,0],[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
λ> zipN (take 2) $ [[1]] ++ repeat []
[[1]]
λ> take 5 $ zipN (take 3) $ repeat [0..1]
[[0,0,0],[1,1,1]]
-}

zipN :: ([a] -> b) -> [[a]] -> [b]
zipN _ [] = []
zipN _ input   | null $ head input = []
zipN foo input =
    let (heads,lefts) = splitLists input
    in foo heads : zipN foo lefts
  where
    splitLists []         = ([], [])
    splitLists ([]:_)     = ([], [])
    splitLists ((a:xs):l) = bimap (a :) (xs :) $ splitLists l

mergeSort :: [Int] -> [Int]
mergeSort a | length a < 2 = a
mergeSort l =
    uncurry merge $
    bimap mergeSort mergeSort $
    splitAt (length l `div` 2) l
  where
    merge a []                = a
    merge [] b                = b
    merge l1@(a:xs) l2@(b:ys) =
        if a < b then a : merge xs l2 else b : merge l1 ys

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n f t = take n . randomRs (f,t) <$> newStdGen

testMerge :: IO ()
testMerge =
    forM_ [0..100] $ \i -> do
        l <- randomIntList i 0 50
        let sorted = mergeSort l
        unless (predicate sorted) $
            error $ "Failed on:\n" ++ show l ++ "\n" ++ show sorted
  where
    predicate []  = True
    predicate [_] = True
    predicate l   =
        all (\i -> (l !! i) <= (l !! (i + 1))) [0..(length l - 2)]

main :: IO ()
main = testMerge
