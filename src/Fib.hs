-- | Copy-paste from
-- https://hackage.haskell.org/package/fibonacci-0.2.0.1/docs/Data-Numbers-Fibonacci.html

module Fib (fib) where


fib :: (Integral int, Num num) => int -> num
fib n | n >= 0    = upperRight $ matrixPower (Matrix 1 1 0) n
      | even n    = negate . fib $ negate n
      | otherwise = fib $ negate n

{-# SPECIALISE fib :: Int -> Int     #-}
{-# SPECIALISE fib :: Int -> Integer #-}

data Matrix a = Matrix a a a

upperRight :: Matrix a -> a
upperRight (Matrix _ a _) = a

-- We implement exponentiation of matrices by repeated squaring.

matrixPower :: (Integral int, Num num) => Matrix num -> int -> Matrix num
matrixPower _ 0             = Matrix 1 0 1
matrixPower m 1             = m
matrixPower m n | r == 0    = square $ matrixPower m q
                | otherwise = times m . square $ matrixPower m q
 where (q,r) = quotRem n 2

{-# SPECIALISE matrixPower :: Matrix Int     -> Int -> Matrix Int     #-}
{-# SPECIALISE matrixPower :: Matrix Integer -> Int -> Matrix Integer #-}

square :: Num num => Matrix num -> Matrix num
square m = times m m

{-# SPECIALISE square :: Matrix Int     -> Matrix Int     #-}
{-# SPECIALISE square :: Matrix Integer -> Matrix Integer #-}

times :: Num num => Matrix num -> Matrix num -> Matrix num
times (Matrix a b c) (Matrix x y z) = Matrix (a*x + by) (a*y + b*z) (by + c*z)
 where by = b*y

{-# SPECIALISE times :: Matrix Int     -> Matrix Int     -> Matrix Int     #-}
{-# SPECIALISE times :: Matrix Integer -> Matrix Integer -> Matrix Integer #-}
