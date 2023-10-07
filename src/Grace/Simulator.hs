-- |

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Grace.Simulator where

import qualified Data.List as List
-- import qualified Data.Ord as Ord
import qualified Data.HashMap.Strict.InsOrd as HashMap
import Data.Text (Text)

-- import Grace.Syntax (Syntax)
import Grace.Value (Value)
import qualified Grace.Value as Value
import qualified Grace.Syntax as Syntax
-- import qualified Grace.Synonym as Synonym


import Control.Lens.Plated (rewrite)

import Debug.Trace (trace)

-- | This value transformation removes all non-json-compatible forms from the
--   encoding of all parts of a network scene. It is implemented this way rather
--   than as an Grace builtin function because I thought using Plated may be an
--   easier way to traverse the deeply nested scene than applying a builtin function
--   through Normalization. (Normalization is how I did in with the Neuron/scene family
--   of functions, and this felt awkward).
--
--   The decision should be revisited once we try it both ways.
toSimulator :: Value -> Value
toSimulator term = rewrite rewriteSubterm term
  where
    rewriteSubterm s = case s of
        Value.Application (Value.Alternative alt) (Value.Record rec) | isTimeConstant alt rec ->
          Just $ Value.Record (HashMap.insert "type" (Value.Scalar (Syntax.Text alt)) rec )
        _ -> Nothing

isTimeConstant :: Text -> HashMap.InsOrdHashMap Text Value -> Bool
isTimeConstant altName rec = trace "CHECKING" $ case altName of
  "Instantaneous" -> HashMap.null rec
  "Sigmoid" -> List.sort (HashMap.keys rec) == ["c_amp", "c_base", "sigma", "v_at_max_tau_mv"]
  "LinearExp" -> List.sort (HashMap.keys rec) == ["coef", "inner_coef", "v_offset_mv"]
  _ -> False
