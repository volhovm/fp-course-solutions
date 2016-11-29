{-# LANGUAGE TemplateHaskell #-}
-- | ??

module Homework10Exe where

import           Data.List  (intercalate)
import           Homework10

printEnvVar

data Kek = Kek
    { lal :: String
    , kek :: Int
    , mem :: Double
    }

genPrettyShow ''Kek



main = "kek"
