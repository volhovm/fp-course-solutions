{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Main (main) where

import           BTree       (BTree (..))
import qualified BTree       as BT hiding (BTree)
import           Data.Kind   ()
import           Data.Monoid ((<>))
import           Data.Proxy  (Proxy (..))

----- TASK 1 -----

newtype Coin color = Coin Int deriving Num

data Blue
data Red

---class OrdT (a :: *) (b :: *) where
class ColorOrd a b where
    compCol :: Proxy a -> Proxy b -> Ordering

instance ColorOrd a b => ColorOrd b a where
    compCol a b = case compCol b a of
        LT -> GT
        GT -> LT
        EQ -> EQ

--instance (ColorOrd a b, ColorOrd b c) => ColorOrd a c where
--    compCol = undefined

instance ColorOrd Blue Red where
    compCol _ _ = LT

class (ColorOrd a b) => Ord' (t :: * -> *) a b where
    compare' :: Proxy a -> Proxy b -> t a -> t b -> Ordering

instance Ord' Coin c1 c2 where
    compare' p1 p2 (Coin a) (Coin b) =
        case compCol p1 p2 of
            EQ -> compare a b
            x  -> x

instance Monoid (Coin a) where
    mempty = Coin 0
    mappend (Coin a) (Coin b) = Coin $ a + b


----- TASK 2 -----

instance (Eq a) => Eq (BTree a) where
    a == b = BT.toList a == BT.toList b

instance (Monoid a, Ord a) => Monoid (BTree a) where
    mempty = BNil
    mappend a BNil = a
    mappend BNil b = b
    mappend lhs@(BNode a l1 r1) rhs@(BNode b l2 r2) =
        case compare a b of
            LT -> let lftm = BT.leftmost rhs
                  in BNode lftm (BT.delete lftm lhs) (BT.delete lftm rhs)
            GT -> rhs <> lhs
            EQ -> BNode a (l1 <> l2) (r1 <> r2)


-------------------

main :: IO ()
main = undefined
