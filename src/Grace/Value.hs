{-# LANGUAGE DerivingStrategies #-}

{-| This module contains the `Value` type used internally for efficient
    evaluation of expressions
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift    #-}

module Grace.Value
    ( -- * Value
      Closure(..)
    , Value(..)
    ) where

import Control.Lens (Plated(..))
import Data.Aeson (FromJSON(..))
import Data.Foldable (toList)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Sequence (Seq)
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Grace.Location (Location)
import Grace.Syntax (Builtin, Operator, Scalar, Syntax)
import Grace.Type (Type)

import qualified Data.Aeson as Aeson
import qualified Data.Sequence as Seq
import qualified Grace.Compat as Compat
import qualified Grace.Syntax as Syntax

{-| A `Closure` captures the current evaluation environment in order to defer
    evaluation until the value of some bound variable is known

    You can think of @Closure name env expression@ as essentially the same thing
    as @\\value -> evaluate ((name, value) : env) e@, except stored using a
    first-order representation.  In fact, you convert to the latter
    representation using `Grace.Normalize.instantiate`.

    This provides efficiency comparable to a higher-order abstract syntax
    tree, except using a first-order representation.
-}
data Closure =
    Closure Text [(Text, Value)] (Syntax Location (Type Location, Value))
    deriving stock (Eq, Show)

{-| This type represents a fully evaluated expression with no reducible
    sub-expressions

    There are two benefits to using a type separate from the surface syntax for
    this purpose:

    * To avoid wastefully reducing the same sub-expression multiple times

    * To use a more efficient representation for reduction purposes
-}
data Value
    = Variable Text Int
      -- ^ The `Data.Text.Text` field is the variable's name (i.e. \"x\")
      --
      -- The `Int` field disambiguates variables with the same name if there are
      -- multiple bound variables of the same name in scope.  Zero refers to the
      -- nearest bound variable and the index increases by one for each bound
      -- variable of the same name going outward.  The following diagram may
      -- help:
      --
      -- >              ┌──refers to──┐
      -- >              │             │
      -- >              v             │
      -- > \x -> \y -> \x     ->    x@0
      -- >
      -- > ┌─────────refers to────────┐
      -- > │                          │
      -- > v                          │
      -- > \x -> \y -> \x     ->    x@1
      --
      -- This `Int` behaves like a De Bruijn index in the spcial case where all
      -- variables have the same name.
      --
      -- You can optionally omit the index if it is 0:
      --
      -- >              ┌─refers to─┐
      -- >              │           │
      -- >              v           │
      -- > \x -> \y -> \x     ->    x
      --
      -- Zero indices are omitted when pretty-printing variables and non-zero
      -- indices appear as a numeric suffix.
    | Lambda Closure
      -- The `Lambda` constructor captures the environment at the time it is
      -- evaluated, so that evaluation can be lazily deferred until the function
      -- input is known.  This is essentially the key optimization that powers
      -- the fast normalization-by-evaluation algorithm.
    | Application Value Value
    | List (Seq Value)
    | Record (InsOrdHashMap Text Value)
    | Field Value Text
    | Alternative Text
    | Merge Value
    | If Value Value Value
    | Builtin Builtin
    | Scalar Scalar
    | Operator Value Operator Value
    | NeuronChannel Value
    | NeuronMembrane Value
    | NeuronNeuron Value
    | NeuronStimulator Value
    | NeuronScene Value
    | NeuronSynapse Value
    | TimeConstant Value
    deriving stock (Eq, Show, Generic)

-- This is not a very good Plated instance - it doesn't look under complicated types,
-- it doesn't go under lambdas. But it should traverse lists and sum and product types
-- well enough to do what we do with it: replace Neuronal structs with their
-- json-friendly counterparts.
instance Plated Value where
  plate f v = case v of
    Variable n i -> pure $ Variable n i
    Lambda c -> pure $ Lambda c
    Application fun a -> Application <$> f fun <*> f a
    List vals -> List <$> traverse f vals
    Record fields -> Record <$> traverse f fields
    Field val name -> Field <$> f val <*> pure name
    Alternative name -> pure $ Alternative name
    Merge val -> Merge <$> f val
    If b l r -> If <$> f b <*> f l <*> f r
    Builtin b -> pure $ Builtin b
    Scalar s -> pure $ Scalar s
    Operator a op b -> Operator <$> f a <*> pure op <*> f b
    NeuronChannel val -> pure $ NeuronChannel val
    NeuronMembrane val -> pure $ NeuronMembrane val
    NeuronNeuron val -> pure $ NeuronNeuron val
    NeuronStimulator val -> pure $ NeuronStimulator val
    NeuronScene val -> pure $ NeuronScene val
    NeuronSynapse val -> pure $ NeuronSynapse val
    TimeConstant val -> pure $ TimeConstant val




instance IsString Value where
    fromString string = Variable (fromString string) 0

instance FromJSON Value where
    parseJSON (Aeson.Object object) = do
        values <- traverse parseJSON object
        pure (Record (Compat.fromAesonMap values))
    parseJSON (Aeson.Array array) = do
        values <- traverse parseJSON array
        pure (List (Seq.fromList (toList values)))
    parseJSON (Aeson.String text) = do
        pure (Scalar (Syntax.Text text))
    parseJSON (Aeson.Number scientific) = do
        pure (Scalar (Syntax.Real scientific))
    parseJSON (Aeson.Bool bool) = do
        pure (Scalar (Syntax.Bool bool))
    parseJSON Aeson.Null = do
        pure (Scalar Syntax.Null)
