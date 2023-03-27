-- |
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift            #-}

module Grace.NeuronNatives where

import Data.Scientific (Scientific)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

data IonLevels = IonLevels {
  ilK :: Scientific,
  ilNa :: Scientific,
  ilCa :: Scientific,
  ilCl :: Scientific
} deriving (Eq, Generic, Show, Lift)
