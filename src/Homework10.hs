{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Solutions for homework 10

module Homework10 where

import qualified Base
import           Control.DeepSeq        (deepseq, ($!!))
import           Control.Lens           hiding ((&))
import           Control.Monad.IO.Class (liftIO)
import           Data.Functor.Const     (Const (..))
import           Data.Functor.Identity  (Identity (..))
import           Data.List              (last, (\\))
import           Data.Maybe             (fromMaybe)
import           Data.Tagged            (Tagged (..))
import qualified Data.Text              as T
import           Data.Tree              (Tree (..))
import           Language.Haskell.TH    (Body (..), Clause (..), Con (..), Dec (..),
                                         Exp (..), Info (..), Lit (..), Name (..),
                                         Pat (..), Q, conT, listE, nameBase, newName,
                                         reify, runIO, varE)
import           System.Directory       (doesDirectoryExist, doesFileExist, listDirectory)
import           System.Environment     (lookupEnv)
import           System.FilePath        (replaceExtension, splitPath, takeExtension,
                                         takeFileName, (</>))
import           System.IO.Unsafe       (unsafePerformIO)
import           Universum

import           Fib                    (fib)

----------------------------------------------------------------------------
-- Task 1
----------------------------------------------------------------------------

-- | Selects elem #m from tuple of size n.
selN :: Int -> Int -> Q Exp
selN m n | m >= n = panic "selN wrong params"
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
    thenvVal <- fromMaybe "Not defined" <$> runIO (lookupEnv "TH_ENV")
    runIO $ putStrLn $ "TH_ENV=" ++ thenvVal
    fooname <- newName "th_env_value"
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
        conName :: [Char]
        conName = nameBase name
        showField :: Name -> Q Exp
        showField name' =
            let fieldName = nameBase name'
            in [|\x -> fieldName ++ " = " ++ show ($(varE name') x)|]
        showFields :: Q Exp
        showFields = listE $ map showField names
    [d|instance Base.Show $(conT name) where
          show x =
              let indent = "    "
                  fieldsShown = intercalate (",\n" ++ indent) (map ($ x) $showFields)
              in concat [ conName
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

type MLens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type MSimple f s a = f s s a a
type MLens' s a = MSimple MLens s a

type MGetting r s a = (a -> Const r a) -> s -> Const r s
type MSetting s t a b = (a -> Identity b) -> s -> Identity t

-- | Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> MLens s t a b
lens get set f s = set s <$> f (get s)

set' :: MSetting s t a b -> b -> s -> t
set' l new = runIdentity . l (Identity . const new)

view' :: MGetting a s a -> s -> a
view' l = getConst . l (\x -> Const x)

over' :: MSetting s t a b -> (a -> b) -> s -> t
over' l c = runIdentity . l (Identity . c)

-- (.~) = set
-- (^.) = flip view
-- (%~) = over

----------------------------------------------------------------------------
-- Task 6
----------------------------------------------------------------------------

-- | VFS representation
data FS
    = Dir { _fname    :: FilePath
          , _contents :: [FS]}
    | File { _fname :: FilePath}
    deriving (Eq)

instance Show FS where
    show (File n) = n
    show o@(Dir n contents) = show' 0 o
      where
        spaces' n = replicate n ' '
        show' n (File a) = spaces' n ++ a
        show' n (Dir a c) =
            concat [spaces' n, a, " ->", concatMap (("\n" ++) . show' (n + 2)) c]

makeLenses ''FS
makePrisms ''FS

isFile, isDir, isEmpty :: FS -> Bool
isFile (File _)  = True
isFile (Dir _ _) = False
isDir = not . isFile
isEmpty (File _)  = True
isEmpty (Dir _ c) = null c

retrieveFS ::  FilePath -> IO FS
retrieveFS fs = do
    ((,) <$> doesFileExist fs <*> doesDirectoryExist fs) >>= \case
        (True,False) -> pure $ File $ takeFileName fs
        (False,True) -> do
            content <- listDirectory fs
            Dir (last $ splitPath fs) <$> mapM (retrieveFS . (</>) fs) content
        _ -> panic $ "retrieveF failed on path: " <> show fs

{-
λ> retrieveFS "/home/volhovm/code/fp-course-solutions/src"
Dir {_name = "src",
     _contents = [ File {_name = "Homework3.hs"},
                   File {_name = "Fib.hs"},
                   File {_name = "Setup.hs"},
                   File {_name = "Homework10.hs"},
                   File {_name = "BTree.hs"},
                   File {_name = "Homework2.hs"},
                   Dir {_name = "kek", _contents = [File {_name = "mda"}]},
                   File {_name = "Main.hs"},
                   File {_name = "Homework10Exe.hs"},
                   File {_name = "Homework1.hs"}]}

λ> k <- retrieveFS "src"
λ> k ^. name
"src"
λ> k ^. contents
[File {_name = "Homework3.hs"},File {_name = "Fib.hs"},File {_name = "Setup.hs"},File {_name = "Homework10.hs"},File {_name = "BTree.hs"},File {_name = "Homework2.hs"},Dir {_name = "kek", _contents = [File {_name = "mda"}]},File {_name = "Main.hs"},File {_name = "Homework10Exe.hs"},File {_name = "Homework1.hs"}]
λ> k ^? _File
Nothing
λ> k ^? _Dir
Just ("src",[File {_name = "Homework3.hs"},File {_name = "Fib.hs"},File {_name = "Setup.hs"},File {_name = "Homework10.hs"},File {_name = "BTree.hs"},File {_name = "Homework2.hs"},Dir {_name = "kek", _contents = [File {_name = "mda"}]},File {_name = "Main.hs"},File {_name = "Homework10Exe.hs"},File {_name = "Homework1.hs"}])

-}

-- Filename -> (FS -> f FS) -> FS -> ???
-- λ> :t lens
-- lens
--   :: Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
-- λ> :t prism
-- prism
--   :: (Choice p, Applicative f) =>
--      (b -> t) -> (s -> Either t a) -> p a (f b) -> p s (f t)

-- | Lens on every subfolder that matches (== dirname)
cd :: FilePath -> Traversal' FS FS
cd dirname =
    contents . traversed . filtered (\x -> isDir x && x ^. fname == dirname)

{-
λ> t <- retrieveFS "."
λ> t ^.. cd "src" . cd "kek"
[Dir {_name = "kek", _contents = []}]
λ> t ^.. cd "src" . ls . fname
["Homework3.hs","Fib.hs","Setup.hs","Homework10.hs","BTree.hs",
"Homework2.hs","kek","Main.hs","Homework10Exe.hs","Homework1.hs"]
λ> t ^.. cd "src" .  file "Setup.hs"
[File {_fname = "Setup.hs"}]
λ> t ^.. cd "src" .  file "Setaoeu"
[]
-}

-- | Lens on directory contents (same as `contents`, almost)
ls :: Traversal' FS FS
ls = contents . each

-- | Lens on every file in dir that matches (== filename)
file :: FilePath -> Traversal' FS FS
file filename =
    contents . traversed . filtered (\x -> isFile x && x ^. fname == filename)

----------------------------------------------------------------------------
-- Task 8
----------------------------------------------------------------------------

files,dirs :: Traversal' FS FS
files = contents . traversed . filtered isFile
dirs = contents . traversed . filtered isFile

changeExtension :: [Char] -> FS -> FS
changeExtension newExt = files . fname %~ flip replaceExtension newExt

{-
λ> t ^?! cd "src"
src ->
  Homework3.hs
  Fib.hs
  Setup.hs
  Homework10.hs
  BTree.hs
  Homework2.hs
  kek ->
  Main.hs
  Homework10Exe.hs
  Homework1.hs
λ> changeExtension "kek" $ t ^?! cd "src"
src ->
  kek ->
  Homework3.kek
  Fib.kek
  Setup.kek
  Homework10.kek
  BTree.kek
  Homework2.kek
  Main.kek
  Homework10Exe.kek
  Homework1.kek
-}

-- | Traversal of all subnames
fnameRec :: Traversal' FS FilePath
fnameRec = getNamesRec False
  where
    getNamesRec :: Bool -> Traversal' FS FilePath
    getNamesRec _ foo o@(File _) = pure o -- not used
    getNamesRec incl foo (Dir name contents) =
        let changedFiles = traverse (fname foo) $ filter isFile contents
            changedDirs = traverse (getNamesRec True foo) $ filter isDir contents
            combined = (++) <$> changedFiles <*> changedDirs
        in (if incl
            then (<*>) (Dir <$> foo name)
            else fmap (Dir name)) combined

{-
λ> t & cd "src" . getNamesRec %~ (++ ".LOL")
... lots of trash
      exclude
    index
    refs ->
      heads ->
        master
      remotes ->
        origin ->
          master
      tags ->
    HEAD
  .gitignore
  src ->
    Homework3.hs.LOL
    Fib.hs.LOL
    Setup.hs.LOL
    Homework10.hs.LOL
    BTree.hs.LOL
    Homework2.hs.LOL
    Main.hs.LOL
    Homework10Exe.hs.LOL
    Homework1.hs.LOL
    kek.LOL ->
-}

-- | Traversal on FS with deleted stuff. First flag is boolean.
rm :: Bool -> [Char] -> FS -> FS
rm _ _ o@(File _) = o
rm force fileName d@(Dir name c) =
    let files' = d ^.. files . filtered (\x -> x ^. fname /= fileName)
        dirs' = d ^.. dirs . filtered
                (\x -> x ^. fname /= fileName && (not (isEmpty x) || force))
    in Dir name $ files' ++ dirs'

{-
λ> t ^?! cd "src" . to (rm True "Main.hs")
src ->
  Homework3.hs
  Fib.hs
  Setup.hs
  Homework10.hs
  BTree.hs
  Homework2.hs
  Homework10Exe.hs
  Homework1.hs
  kek ->
-}


-- | Lawless move that accumulates relative path
move :: FilePath -> Traversal' FS FS
move name foo o@(File n) | n == name = foo o
                         | otherwise = pure o
move name foo (Dir n contents) =
    let p = map (\x -> x & fname %~ (n </>))
        matched = p $ filter matches contents
        others = p $ filter (not . matches) contents
    in Dir name . (others ++) <$> traverse foo matched
  where
    matches x = x ^. fname  == name

{-
λ> t ^?! move "src" . move "kek" . move "MDA"
./src/kek/MDA
-}

-- Was not used
fsPath :: FS -> [[Char]]
fsPath (File n)  = [n]
fsPath (Dir n c) = concatMap (map (n </>) . fsPath) c

----------------------------------------------------------------------------
-- Task 9
----------------------------------------------------------------------------

type Walker = StateT [(Int, Int, [Char])] (ReaderT FS IO)

getWalkerFS :: forall f . Applicative f => Walker (LensLike' f FS FS)
getWalkerFS = do
    k <- use $ to $ map (view _3)
    pure $ applyAll $ reverse k
  where
    applyAll []     = identity
    applyAll (x:xs) = cd x . applyAll xs

printStats :: Walker ()
printStats = do
    (i,j,_) <- uses identity $ fromMaybe (0,0,"") . head
    p <- uses identity $ map $ view _3 -- better?
    start <- views fname T.pack
    putText $ "You're in " <>
        T.intercalate "/" (start : reverse (map T.pack p))
    putText $ "Files from root: " <> show i
    putText $ "Dirs from root: " <> show j

walkerMain :: Walker ()
walkerMain = do
    putText "\n-----------"
    printStats
    liftIO $ putStr ("> " :: Text)
    root <- view identity
    cmd <- getLine
    case words cmd of
      ["cd",to'] -> do
          let subdir = T.unpack to'
          f <- getWalkerFS
          if isJust $ root ^? f . cd subdir
          then do
              f' <- getWalkerFS -- conflict Endo/First with 'f'
              putText "Let's go ok"
              let files', dirs' :: Int
                  files' = length $ root ^.. f' . files
                  dirs' = length $ root ^.. f' . dirs
              identity %= \case
                 [] -> [(files',dirs',subdir)]
                 xs@((f,d,_):_) -> (files'+f,dirs'+d,subdir):xs
          else putText "Won't go, didn't find such dir"
      ["up"] -> putText "Going up" >> identity %= drop 1
      ["ls"] -> do
          f <- getWalkerFS
          putText $ T.intercalate "\n" $ root ^.. f . ls . fname . to T.pack
      _    -> putText "Wrong command"
    walkerMain

runWalker :: [Char] -> IO ()
runWalker startPath = do
    fsStart <- retrieveFS startPath
    void $ runReaderT (runStateT walkerMain []) fsStart

{-
λ> runWalker "/home/volhovm/code/fp-course-solutions"

-----------
You're in fp-course-solutions
Files from root: 0
Dirs from root: 0
> ls
shell.nix
.stack-work
src
fp-course-solutions.cabal
ChangeLog.md
stack.yaml
LICENSE
.git
.gitignore

-----------
You're in fp-course-solutions
Files from root: 0
Dirs from root: 0
> cd src
Let's go ok

-----------
You're in fp-course-solutions/src
Files from root: 6
Dirs from root: 3
> ls
Homework3.hs
Fib.hs
Setup.hs
Homework10.hs
BTree.hs
Homework2.hs
kek
Main.hs
Homework10Exe.hs
Homework1.hs

-----------
You're in fp-course-solutions/src
Files from root: 6
Dirs from root: 3
> cd kek
Let's go ok

-----------
You're in fp-course-solutions/src/kek
Files from root: 15
Dirs from root: 4
> ls
MDA

-----------
You're in fp-course-solutions/src/kek
Files from root: 15
Dirs from root: 4
>
-}

----------------------------------------------------------------------------
-- Task 10
----------------------------------------------------------------------------

type Iso2 b a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p b (f b)

-- I just used type holes to complete this definition in 10s
iso' :: (b -> a) -> (a -> b) -> Iso2 b a
iso' from to = dimap from (fmap to)

-- p1 :: forall p f. (Profunctor p, Functor f) => p a (f a) -> p b (f b)
-- p1 :: forall p f. (Functor f) => (a -> f a) -> (b -> f b)
-- p1 :: forall p f. (Functor f) => Tagged * (f a) -> Tagged * (f b)
from' :: Iso2 b a -> Iso2 a b
from' p1 =
    dimap
    (\a -> unTagged $ unTagged $ p1 $ Tagged $ Tagged a)
    (\fb -> fmap getConst $ p1 Const <$> fb)

-- | Isomorphism between tree and FS
treeFsIso :: Iso2 (Tree [Char]) FS
treeFsIso p1 = dimap fromTree (fmap toTree) p1
  where
    fromTree Node{..} | null subForest = File rootLabel
    fromTree Node{..} = Dir rootLabel $ map fromTree subForest
    toTree (File n)  = Node n []
    toTree (Dir n c) = Node n $ map toTree c

{-
λ> t <- retrieveFS "/home/volhovm/code/fp-course-solutions"
λ> t ^?! cd "src"
src ->
  Homework3.hs
  Fib.hs
  Setup.hs
  Homework10.hs
  BTree.hs
  Homework2.hs
  kek ->
    MDA
  Main.hs
  Homework10Exe.hs
  Homework1.hs
λ> (t ^?! cd "src") ^. from treeFsIso
Node {rootLabel = "src", subForest = [Node {rootLabel = "Homework3.hs", subForest = []},Node {rootLabel = "Fib.hs", subForest = []},Node {rootLabel = "Setup.hs", subForest = []},Node {rootLabel = "Homework10.hs", subForest = []},Node {rootLabel = "BTree.hs", subForest = []},Node {rootLabel = "Homework2.hs", subForest = []},Node {rootLabel = "kek", subForest = [Node {rootLabel = "MDA", subForest = []}]},Node {rootLabel = "Main.hs", subForest = []},Node {rootLabel = "Homework10Exe.hs", subForest = []},Node {rootLabel = "Homework1.hs", subForest = []}]}
-}
