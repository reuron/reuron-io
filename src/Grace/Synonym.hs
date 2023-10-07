-- | Handle the relationship between records and their semantic counterparts
--
-- There are certain records in the grace language that we distinguish from
-- others as being meaningful NeuronBench entities, like a Channel, a Neuron,
-- etc.
--
-- This module helps with two things:
--   - Desugaring user specified semantic types and values into grace types and terms.
--   - Pretty printing grace types and terms as semantic types when possible.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Grace.Synonym where

import Control.Lens (anyOf)
import Control.Lens.Plated (rewrite, plate)
import Data.Text (Text)
import qualified Data.List as List

import Grace.Syntax (Syntax)
import qualified Grace.Monotype as Monotype
import qualified Grace.Syntax as Syntax
import qualified Grace.Type as GraceType

import Grace.Location (Offset(..))

-- import Debug.Trace (trace)

-- | The main export of this module - it replaces all Semantic types
-- in a term with the corresponding structure.
desugarTerm :: Syntax s a -> Syntax s a
desugarTerm term = rewrite rewriteSubterm term

rewriteSubterm :: Syntax s a -> Maybe (Syntax s a)
rewriteSubterm subterm = case subterm of
  Syntax.Lambda {} -> Nothing
  Syntax.Application {} -> Nothing
  Syntax.Variable {} -> Nothing
  Syntax.Let {} -> Nothing
  Syntax.Field {} -> Nothing
  Syntax.Alternative {} -> Nothing
  Syntax.List {} -> Nothing
  Syntax.Merge {} -> Nothing
  Syntax.Scalar {} -> Nothing
  Syntax.If {} -> Nothing
  Syntax.Builtin {} -> Nothing
  Syntax.Operator {} -> Nothing
  Syntax.Embed {} -> Nothing
  Syntax.Record {} -> Nothing
  Syntax.Annotation { annotation, .. }
    | isDeeplySemantic annotation
      -> Just $ Syntax.Annotation { annotation = rewrite rewriteType annotation, .. }
  Syntax.Annotation {} -> Nothing

isDeeplySemantic :: GraceType.Type s -> Bool
isDeeplySemantic ty = isSemantic ty || anyOf plate isSemantic ty

isSemantic :: GraceType.Type s -> Bool
isSemantic (GraceType.Semantic {}) = True
isSemantic _ = False

rewriteType :: GraceType.Type s -> Maybe (GraceType.Type s)
rewriteType ty = case ty of
  GraceType.Semantic { location, semanticLabel } ->
    Just $ case semanticLabel of
      GraceType.LabelChannel -> GraceType.channelRecord location
      GraceType.LabelMembrane -> GraceType.membraneRecord location
      GraceType.LabelMembraneChannel -> GraceType.membraneChannelRecord location
      GraceType.LabelNeuron -> GraceType.neuronRecord location
      GraceType.LabelSynapse -> GraceType.synapseRecord location
      GraceType.LabelScene -> GraceType.sceneRecord location
      GraceType.LabelTest -> GraceType.testRecord location
  _ -> Nothing

-- -- One-off test helpers.
t :: GraceType.Type Offset
t = GraceType.List
  { type_ = GraceType.Semantic
    { semanticLabel = GraceType.LabelTest
    , location = Offset 0
    }
  , location = Offset 0
  }

u :: GraceType.Type Offset
u = GraceType.Record
  { fields = GraceType.Fields
    [("foo", GraceType.Scalar (Offset 0) (Monotype.Real))
    ,("bar", GraceType.Scalar (Offset 0) (Monotype.Real))
    ] Monotype.EmptyFields, location = Offset 0
  }
--
-- term =
--   let
--     location = Offset 0
--     scalar = Syntax.Scalar { scalar = Syntax.Real 1.0, location }
--     untyped =
--       Syntax.Record
--       { location
--       , fieldValues = [("foo", scalar), ("bar", scalar)]
--       }
--   in
--     Syntax.Annotation
--       { location
--       , annotation = t
--       , annotated = untyped
--       }
--
-- term2 =
--   Syntax.Annotation
--     {location = 0
--     , annotated = Syntax.Record
--       {location = 0
--       , fieldValues =
--         [("foo",Syntax.Scalar
--            {location = 7
--            , scalar = Syntax.Real 1.0}),
--           ("bar",Syntax.Scalar {location = 17, scalar = Syntax.Real 1.0})]
--       }
--     , annotation = GraceType.Semantic {location = 0
--                                       , semanticLabel = GraceType.LabelTest}}
--
-- t2 =GraceType.Semantic {location = 25
--                                       , semanticLabel = GraceType.LabelTest}

resugarType :: Show s => GraceType.Type s -> GraceType.Type s
resugarType ty = rewrite rewriteType' ty
  where
    isRecord :: GraceType.Type s -> GraceType.Type s -> Bool
    isRecord (GraceType.Record { fields =  GraceType.Fields flds _ }) tmpl = recordFieldsSatisfy flds tmpl
    isRecord _ _ = False

    rewriteType' r@(GraceType.Record {location}) | isRecord r (GraceType.testRecord location) =

        Just $ GraceType.Semantic { semanticLabel = GraceType.LabelTest, location }
    rewriteType' _ = Nothing

-- | The record fields of the first argument are a close enough match to the
-- type given in the right argument that the type in the right argument may
-- be substituted during pretty-printing.
recordFieldsSatisfy :: [(Text, GraceType.Type s)] -> GraceType.Type s -> Bool
recordFieldsSatisfy
  fields
  (GraceType.Record
    { GraceType.fields = GraceType.Fields templateFields Monotype.EmptyFields }
  ) = lengthMatches && namesMatch && typesMatch
  where
    lengthMatches = length fields == length templateFields
    namesMatch = all id $ zipWith (==) (List.sort $ fst <$> fields) (List.sort $ fst <$> templateFields)
    typesMatch = True -- TODO
recordFieldsSatisfy _ _ =
  error "Internal error: recordFieldsSatisfy must only be called with a monomorphic record for a template."
