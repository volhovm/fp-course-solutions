{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Solutions for homework 10

module Homework10 where

import           Control.DeepSeq        (deepseq, ($!!))
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor.Const     (Const (..))
import           Data.Functor.Identity  (Identity (..))
import           Data.Maybe             (fromMaybe)
import           Language.Haskell.TH    (Body (..), Clause (..), Con (..), Dec (..),
                                         Exp (..), Info (..), Lit (..), Name (..),
                                         Pat (..), Q, conT, listE, nameBase, newName,
                                         reify, varE)
import           System.Environment     (lookupEnv)
import           System.IO.Unsafe       (unsafePerformIO)

import           Fib                    (fib)

----------------------------------------------------------------------------
-- Task 1
----------------------------------------------------------------------------

-- | Selects elem #m from tuple of size n.
selN :: Int -> Int -> Q Exp
selN m n | m >= n = error "selN wrong params"
selN m n = do
    x <- newName "x"
    pure $ LamE [TupP (insWild m ++ [VarP x] ++ insWild (n - m - 1))] $ VarE x
  where
    insWild k = replicate k WildP

{-
λ> $(selN 2 3) (1,2,"kek")
"kek"
λ> $(selN 1 3) (1,2,"kek")
2
λ> $(selN 0 3) (1,2,"kek")
1
-}

----------------------------------------------------------------------------
-- Task 2
----------------------------------------------------------------------------

-- | Generates single function @th_env_value :: String@ that outputs
-- the content of environment variable TH_ENV that was retrieved at
-- the time of compilation or "Not defined" if it wasn,t defined.
printEnvVar :: Q [Dec]
printEnvVar = do
    let thenvVal :: String
        thenvVal = fromMaybe "Not defined" $ unsafePerformIO (lookupEnv "TH_ENV")
        kek = id $!! unsafePerformIO (putStrLn $ "TH_ENV=" ++ thenvVal)
    fooname <- kek `deepseq` newName "th_env_value"
    pure $ [FunD fooname [Clause [] (NormalB $ LitE $ StringL thenvVal) []]]

{-
ξ> ghc -outputdir /tmp/ Homework10Exe.hs
[1 of 2] Compiling Homework10       ( Homework10.hs, /tmp/Homework10.o )
[2 of 2] Compiling Homework10Exe    ( Homework10Exe.hs, /tmp/Homework10Exe.o )
TH_ENV=Not defined
ξ> export TH_ENV="domen kozar"
12:39:07 [volhovm@avishai] ~/code/fp-course-solutions (master)
ξ> ghc -outputdir /tmp/ Homework10Exe.hs
[1 of 2] Compiling Homework10       ( Homework10.hs, /tmp/Homework10.o )
[2 of 2] Compiling Homework10Exe    ( Homework10Exe.hs, /tmp/Homework10Exe.o ) [TH]
TH_ENV=domen kozar

-}

----------------------------------------------------------------------------
-- Task 3
----------------------------------------------------------------------------

-- | Generates pretty show for records.
genPrettyShow :: Name -> Q [Dec]
genPrettyShow name = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name
    let names = map (\(name,_,_) -> name) fields
        conName :: String
        conName = nameBase name
        showField :: Name -> Q Exp
        showField name' =
            let fieldName = nameBase name'
            in [|\x -> fieldName ++ " = " ++ show ($(varE name') x)|]
        showFields :: Q Exp
        showFields = listE $ map showField names
    [d|instance Show $(conT name) where
          show x =
              let indent = "    "
                  fieldsShown = intercalate (",\n" ++ indent) (map ($ x) $showFields)
              in mconcat [ conName
                         , " {\n"
                         , indent
                         , fieldsShown
                         , "\n}" ]
          |]

{-
data Kek = Kek
    { lal :: String
    , kek :: Int
    , mem :: Double
    }

genPrettyShow ''Kek

λ> Kek "heh" 3 5.0
Kek {
    lal = "heh",
    kek = 3,
    mem = 5.0
}
-}

----------------------------------------------------------------------------
-- Task 4
----------------------------------------------------------------------------

-- | Effective compile-time fibonnaci number. Але навіщо??
fibonacciTH :: Integer -> Q Exp
fibonacciTH n = [|(fib n :: Integer)|]

{-
λ> fib 123
22698374052006863956975682
-}

----------------------------------------------------------------------------
-- Task 5
----------------------------------------------------------------------------

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Simple f s a = f s s a a
type Lens' s a = Simple Lens s a

type Getting r s a = (a -> Const r a) -> s -> Const r s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

-- | Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f s = set s <$> f (get s)

set' :: Setting s t a b -> b -> s -> t
set' l new = runIdentity . l (Identity . const new)

view' :: Getting a s a -> s -> a
view' l = getConst . l (\x -> Const x)

over' :: Setting s t a b -> (a -> b) -> s -> t
over' l c = runIdentity . l (Identity . c)

-- (.~) = set
-- (^.) = flip view
-- (%~) = over

----------------------------------------------------------------------------
-- Task 6
----------------------------------------------------------------------------

-- | VFS representation
data FS
    = Dir { name     :: FilePath
          , contents :: [FS]}
    | File { name :: FilePath}
    deriving (Show)


{-

* type MySas a = StateT (ProgState) (ReaderT Env IO) a
* instance MonadReader Env MySas where ...
* instance MonadState ProgState MySas where ...
--* instance MonadState (Map . .) MySas where ...
--* instance MonadState (Map a b, ProgState) MySas where ...
--  get = do
--    a <- foo $ ask -- foo :: IORef -> Map
--    b <- lift $ get

kek :: (MonadState s m, HasA s, HasB s) => m b
kek = do
  a <- view fieldA
  b <- view fieldB

-}
