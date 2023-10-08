{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module contains the logic for efficiently evaluating an expression
module Grace.Normalize
    ( -- * Normalization
      evaluate
    , quote
    , apply
    ) where

import Data.Scientific (Scientific, fromFloatDigits, toRealFloat)
import Data.Sequence (Seq(..), ViewL(..))
import Data.Text (Text)
import Data.Void (Void)
import Grace.Location (Location)
import Grace.Syntax (Builtin(..), Scalar(..), Syntax)
import Grace.Type (Type)
import Grace.Value (Closure(..), Value)
import Prelude hiding (succ)

import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Grace.Syntax as Syntax
import qualified Grace.Value as Value

{- $setup

   >>> :set -XOverloadedStrings
-}

{-| Lookup a variable from an ordered environment of name-value pairs using the
    variable's name and index
-}
lookupVariable
    :: Text
    -- ^ Variable name
    -> Int
    -- ^ Variable index
    -> [(Text, Value)]
    -- ^ Evaluation environment
    -> Value
lookupVariable name index environment =
    case environment of
        (key, value) : rest ->
            if name == key
            then if index == 0
                 then value
                 else lookupVariable name (index - 1) rest
            else lookupVariable name index rest
        [] ->
            -- In the `Value` type, free variables are stored using negative
            -- indices (starting at -1) to avoid collision with bound variables
            --
            -- >>> evaluate [] "x"
            -- Variable "x" (-1)
            --
            -- This has the nice property that `quote` does the right thing when
            -- converting back to the `Syntax` type.
            Value.Variable name (negate index - 1)

{-| Substitute an expression into a `Closure`

    > instantiate (Closure name env expression) value =
    >    evaluate ((name, value) : env) expression
-}
instantiate :: Closure -> Value -> Value
instantiate (Closure name env syntax) value =
    evaluate ((name, value) : env) syntax

asInteger :: Scalar -> Maybe Integer
asInteger (Natural n) = Just (fromIntegral n)
asInteger (Integer n) = Just n
asInteger  _          = Nothing

asReal :: Scalar -> Maybe Scientific
asReal (Natural n) = Just (fromIntegral n)
asReal (Integer n) = Just (fromInteger  n)
asReal (Real    n) = Just n
asReal  _          = Nothing

{-| Evaluate an expression, leaving behind a `Value` free of reducible
    sub-expressions

    This function uses separate types for the input (i.e. `Syntax`) and the
    output (i.e. `Value`) in order to avoid wastefully evaluating the same
    sub-expression multiple times.
-}
evaluate
    :: [(Text, Value)]
    -- ^ Evaluation environment (starting at @[]@ for a top-level expression)
    -> Syntax Location (Type Location, Value)
    -- ^ Surface syntax
    -> Value
    -- ^ Result, free of reducible sub-expressions
evaluate env syntax =
    case syntax of
        Syntax.Variable{..} ->
            lookupVariable name index env

        Syntax.Application{..} -> apply function' argument'
          where
            function' = evaluate env function
            argument' = evaluate env argument

        Syntax.Lambda{..} ->
            Value.Lambda (Closure name env body)

        Syntax.Annotation{..} ->
            evaluate env annotated

        Syntax.Let{..} ->
            evaluate (foldl snoc env bindings) body
          where
            snoc environment Syntax.Binding{ name, assignment} =
                (name, evaluate environment assignment) : environment

        Syntax.List{..} ->
            Value.List (fmap (evaluate env) elements)

        Syntax.Record{..} ->
            Value.Record (HashMap.fromList (map adapt fieldValues))
          where
            adapt (key, value) = (key, evaluate env value)

        Syntax.Field{..} ->
            case evaluate env record of
                Value.Record fieldValues
                    | Just value <- HashMap.lookup field fieldValues ->
                        value
                other ->
                    Value.Field other field

        Syntax.Alternative{..} ->
            Value.Alternative name

        Syntax.Merge{..} ->
            Value.Merge (evaluate env handlers)

        Syntax.If{..} ->
            case predicate' of
                Value.Scalar (Bool True) -> ifTrue'
                Value.Scalar (Bool False) -> ifFalse'
                _ -> Value.If predicate' ifTrue' ifFalse'
          where
            predicate' = evaluate env predicate
            ifTrue'    = evaluate env ifTrue
            ifFalse'   = evaluate env ifFalse

        Syntax.Scalar{..} ->
            Value.Scalar scalar

        Syntax.Operator{ operator = Syntax.And, .. } ->
            case left' of
                Value.Scalar (Bool True) -> right'
                Value.Scalar (Bool False) -> Value.Scalar (Bool False)
                _ -> case right' of
                    Value.Scalar (Bool True) -> left'
                    Value.Scalar (Bool False) -> Value.Scalar (Bool False)
                    _ -> Value.Operator left' Syntax.And right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Or, .. } ->
            case left' of
                Value.Scalar (Bool True) -> Value.Scalar (Bool True)
                Value.Scalar (Bool False) -> right'
                _ -> case right' of
                    Value.Scalar (Bool True) -> Value.Scalar (Bool True)
                    Value.Scalar (Bool False) -> left'
                    _ -> Value.Operator left' Syntax.Or right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Times, .. } ->
            case (left', right') of
                (Value.Scalar (Natural 1), _) ->
                    right'
                (Value.Scalar (Natural 0), _) ->
                    Value.Scalar (Natural 0)
                (_, Value.Scalar (Natural 1)) ->
                    left'
                (_, Value.Scalar (Natural 0)) ->
                    Value.Scalar (Natural 0)
                (Value.Scalar l, Value.Scalar r)
                    | Natural m <- l
                    , Natural n <- r ->
                        Value.Scalar (Natural (m * n))
                    | Just m <- asInteger l
                    , Just n <- asInteger r ->
                        Value.Scalar (Integer (m * n))
                    | Just m <- asReal l
                    , Just n <- asReal r ->
                        Value.Scalar (Real (m * n))
                _ ->
                    Value.Operator left' Syntax.Times right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Plus, .. } ->
            case (left', right') of
                (Value.Scalar (Natural 0), _) ->
                    right'
                (_, Value.Scalar (Natural 0)) ->
                    left'
                (Value.Scalar (Text ""), _) ->
                    right'
                (_, Value.Scalar (Text "")) ->
                    left'
                (Value.List [], _) ->
                    right'
                (_, Value.List []) ->
                    left'
                (Value.Scalar l, Value.Scalar r)
                    | Natural m <- l
                    , Natural n <- r ->
                        Value.Scalar (Natural (m + n))
                    | Just m <- asInteger l
                    , Just n <- asInteger r ->
                        Value.Scalar (Integer (m + n))
                    | Just m <- asReal l
                    , Just n <- asReal r ->
                        Value.Scalar (Real (m + n))
                    | Text m <- l
                    , Text n <- r ->
                        Value.Scalar (Text (m <> n))
                (Value.List l, Value.List r) ->
                    Value.List (l <> r)
                _ ->
                    Value.Operator left' Syntax.Plus right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Minus, .. } ->
            case (left', right') of
                (Value.Scalar l, Value.Scalar r)
                    | Natural m <- l
                    , Natural n <- r ->
                        Value.Scalar (Natural (m - n))
                    | Just m <- asInteger l
                    , Just n <- asInteger r ->
                        Value.Scalar (Integer (m - n))
                    | Just m <- asReal l
                    , Just n <- asReal r ->
                        Value.Scalar (Real (m - n))
                _ ->
                    Value.Operator left' Syntax.Minus right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Operator{ operator = Syntax.Divide, .. } ->
            case (left', right') of
                (Value.Scalar l, Value.Scalar r)
                    | Natural m <- l
                    , Natural n <- r ->
                        Value.Scalar (Natural (div m n))
                    | Just m <- asInteger l
                    , Just n <- asInteger r ->
                        Value.Scalar (Integer (div m n))
                    | Just m <- asReal l
                    , Just n <- asReal r ->
                        Value.Scalar (Real (m / n))
                _ ->
                    Value.Operator left' Syntax.Minus right'
          where
            left'  = evaluate env left
            right' = evaluate env right

        Syntax.Builtin{ builtin = RealPi } -> Value.Scalar (Real 3.14159)
        Syntax.Builtin{..} ->
            Value.Builtin builtin

        Syntax.Embed{ embedded = (_, value) } ->
            value

{-| This is the function that implements function application, including
    evaluating anonymous functions and evaluating all built-in functions.
-}
apply :: Value -> Value -> Value
apply (Value.Lambda (Closure name capturedEnv body)) argument =
    evaluate ((name, argument) : capturedEnv) body
apply
    (Value.Merge (Value.Record alternativeHandlers))
    (Value.Application (Value.Alternative alternative) x)
    | Just f <- HashMap.lookup alternative alternativeHandlers =
        apply f x
apply
    (Value.Application (Value.Builtin ListDrop) (Value.Scalar (Natural n)))
    (Value.List elements) =
        Value.List (Seq.drop (fromIntegral n) elements)
apply
    (Value.Application (Value.Builtin ListTake) (Value.Scalar (Natural n)))
    (Value.List elements) =
        Value.List (Seq.take (fromIntegral n) elements)
apply (Value.Builtin ListHead) (Value.List []) =
    Value.Application (Value.Alternative "None") (Value.Record [])
apply (Value.Builtin ListHead) (Value.List (x :<| _)) =
    Value.Application (Value.Alternative "Some") x
apply (Value.Builtin ListLast) (Value.List []) =
    Value.Application (Value.Alternative "None") (Value.Record [])
apply (Value.Builtin ListLast) (Value.List (_ :|> x)) =
    Value.Application (Value.Alternative "Some") x
apply (Value.Builtin ListReverse) (Value.List xs) =
    Value.List (Seq.reverse xs)
apply
    (Value.Application
        (Value.Application (Value.Builtin ListEqual) f)
        (Value.List rs)
    )
    (Value.List ls)
        | length ls /= length rs =
            Value.Scalar (Bool False)
        | Just bools <- traverse toBool (Seq.zipWith equal ls rs) =
        Value.Scalar (Bool (and bools))
      where
        toBool (Value.Scalar (Bool b)) = Just b
        toBool  _                      = Nothing

        equal l r = apply (apply f l) r
apply
    (Value.Application
        (Value.Builtin ListFold)
        (Value.Record
            (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                [ ("cons"  , cons)
                , ("nil"   , nil)
                ]
            )
        )
    )
    (Value.List elements) = loop (Seq.reverse elements) nil
  where
    loop xs !result =
        case Seq.viewl xs of
            EmptyL  -> result
            y :< ys -> loop ys (apply (apply cons y) result)
apply (Value.Builtin ListIndexed) (Value.List elements) =
    Value.List (Seq.mapWithIndex adapt elements)
  where
    adapt index value =
        Value.Record
            [ ("index", Value.Scalar (Natural (fromIntegral index)))
            , ("value", value)
            ]
apply (Value.Builtin ListLength) (Value.List elements) =
    Value.Scalar (Natural (fromIntegral (length elements)))
apply
    (Value.Application (Value.Builtin ListMap) f)
    (Value.List elements) =
        Value.List (fmap (apply f) elements)
apply
    (Value.Application
        (Value.Application
            (Value.Builtin NaturalFold)
            (Value.Scalar (Natural n))
        )
        succ
    )
    zero =
        go n zero
  where
    go 0 !result = result
    go m !result = go (m - 1) (apply succ result)
apply
    (Value.Application
        (Value.Builtin NaturalEqual)
        (Value.Scalar (Natural m))
    )
    (Value.Scalar (Natural n)) = Value.Scalar (Bool $ m == n)
apply
    (Value.Application
        (Value.Builtin NaturalMod)
        (Value.Scalar (Natural m))
    )
    (Value.Scalar (Natural n)) = Value.Scalar (Natural $ m `mod` n)
apply
    (Value.Builtin NaturalToInteger)
    (Value.Scalar (Natural n)) = Value.Scalar (Integer (fromIntegral n))
apply
    (Value.Builtin Channel)
    (Value.Record
      (List.sortBy (Ord.comparing fst) . HashMap.toList ->
        [("activation", activation)
        ,("inactivation", inactivation)
        ,("ion_selectivity", ions)
        ]
      )
    )
     = Value.Channel $ Value.Record (HashMap.fromList
                     [("ion_selectivity", ions)
                     ,("activation", convertActivation activation)
                     ,("inactivation", convertActivation inactivation)])
  where
    convertActivation (Value.Scalar Null) = Value.Scalar Null
    convertActivation (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                       [("gates", gates)
                       ,("magnitude", magnitude)
                       ,("time_constant", timeConstant)
                       ])) = Value.Record (HashMap.fromList
                                           [("gates", gates)
                                           ,("magnitude", magnitude)
                                           ,("time_constant", convertTimeConstant timeConstant)
                                           ])
    convertActivation x = error $ "Encountered non-record activation: " ++ show x

    -- Time constant variant is one of the recognized types of time constant function.
    isAlt :: Text.Text -> Bool
    isAlt alt = alt == "Sigmoid" || alt ==  "LinearExp" || alt == "Instantaneous"

    convertTimeConstant (Value.Application (Value.Alternative altName) (Value.Record fields)) | isAlt altName =
      Value.Record ( HashMap.insert "type" (Value.Scalar (Text altName)) fields )
    convertTimeConstant x = error $ "Encountered invalid timeConstant: " ++ show x

apply
    (Value.Builtin Channel)
    x = error $ "applying Channel to " ++ show x
apply
    (Value.Builtin Membrane)
    (Value.Record
      (List.sortBy (Ord.comparing fst) . HashMap.toList ->
       [("capacitance_farads_per_square_cm", capacitance)
       ,("membrane_channels", Value.List membraneChannels)
       ]
      )
    ) = Value.Record (HashMap.fromList
                      [("capacitance_farads_per_square_cm", capacitance)
                      ,("membrane_channels", Value.List $ handleMembraneChannel <$> membraneChannels)
                      ]
                     )
        where
          handleMembraneChannel :: Value -> Value
          handleMembraneChannel
            (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                           [("channel", channel)
                           ,("siemens_per_square_cm", conductance)
                           ])) = Value.Record
                                 [("channel", channel)
                                 ,("siemens_per_square_cm", conductance)]
          handleMembraneChannel x = error $ "Encountered unexpected membraneChannel: " ++ show x
apply (Value.Builtin Membrane) x = error $ "Encountered unexpected Membrane: " ++ show x

apply
  (Value.Builtin Neuron)
  (Value.Record
    (List.sortBy (Ord.comparing fst) . HashMap.toList ->
      [("membranes", membranes)
      ,("segments", segments)
      ]
    )
  ) = Value.Record (HashMap.fromList
      [("membranes", membranes)
      ,("segments", segments)])
apply (Value.Builtin Neuron) x = error (show x) -- TODO

apply
  (Value.Builtin Stimulator)
  (Value.Record
    (List.sortBy (Ord.comparing fst) . HashMap.toList ->
      [("current_shape", currentShape)
      ,("envelope", envelope)
      ]
    )
  ) = Value.Stimulator $ Value.Record (HashMap.fromList
                                             [("current_shape", convertCurrentShape currentShape)
                                             ,("envelope", envelope)
                                             ])
      where
        convertCurrentShape (Value.Application (Value.Alternative altName) (Value.Record fields)) | isAlt altName =
                                              Value.Record (HashMap.insert "type" (Value.Scalar (Text altName)) fields)
        convertCurrentShape x = error (show x)

        isAlt :: Text.Text -> Bool
        isAlt alt = alt == "SquareWave" || alt == "LinearRamp" || alt == "FrequencyRamp"

apply (Value.Builtin Stimulator) x = error (show x) -- TODO

apply (Value.Builtin Scene)
  (Value.Record
  (List.sortBy (Ord.comparing fst) . HashMap.toList ->
   [("neurons", neurons), ("synapses", synapses)])) =
  Value.Scene $ Value.Record (HashMap.fromList
                                    [ ("neurons", convertNeurons neurons)
                                    , ("synapses", synapses)
                                    ])
  where
    convertNeurons (Value.List neuronsAndStimulators) = Value.List (convertNeuronAndStimulator <$> neuronsAndStimulators)
    convertNeurons x = error (show x)

    convertNeuronAndStimulator
      (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                  [ ("location", location)
                  , ("neuron", neuron)
                  ,("stimulator_segments", stimulatorSegment)])) =
      Value.Record (HashMap.fromList
                    [ ("location", location)
                    , ("neuron", neuron)
                    , ("stimulator_segments" , stimulatorSegment)
                    ])
    convertNeuronAndStimulator (Value.Record r) = error $ show $ HashMap.keys r
    convertNeuronAndStimulator x = error (show x)
apply (Value.Builtin Scene) (Value.Record x) = error $ show $ HashMap.keys x
apply (Value.Builtin Scene) x = error (show x)

apply (Value.Builtin Synapse)
  (Value.Record
  (List.sortBy (Ord.comparing fst) . HashMap.toList ->
   [("post_neuron", postNeuron)
   ,("post_segment", postSegment)
   ,("pre_neuron", preNeuron)
   ,("pre_segment", preSegment)
   ,("synapse_membranes", synapseMembranes)
   ])) = Value.Synapse $ Value.Record
  (HashMap.fromList
    [("pre_neuron", preNeuron)
    ,("pre_segment", preSegment)
    ,("post_neuron", postNeuron)
    ,("post_segment", postSegment)
    ,("synapse_membranes", convertSynapseMembranes synapseMembranes)])
  where

    convertSynapseMembranes (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                             [("cleft_solution", cleftSolution)
                             ,("postsynaptic_receptors", Value.List postsynapticReceptors)
                             ,("presynaptic_pumps", Value.List presynapticPumps)
                             ,("surface_area_square_mm", surfaceAreaSquareMM)
                             ,("transmitter_concentrations", transmitterConcentrations)
                             ])) =
      Value.Record
      (HashMap.fromList
                   [("cleft_solution", cleftSolution)
                   ,("transmitter_concentrations", transmitterConcentrations)
                   ,("presynaptic_pumps", Value.List $ convertPresynapticPump <$> presynapticPumps)
                   ,("postsynaptic_receptors", Value.List $ convertPostsynapticReceptor <$> postsynapticReceptors)
                   ,("surface_area_square_mm", surfaceAreaSquareMM)
                   ])
    convertSynapseMembranes x = error (show x)

    convertPresynapticPump
        (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                       [("transmitter", transmitter)
                       ,("transmitter_pump_params", transmitterPumpParams)
                       ])) = Value.Record $ HashMap.fromList
                             [("transmitter", convertTransmitter transmitter)
                             ,("transmitter_pump_params", convertTransmitterPumpParams transmitterPumpParams)]
    convertPresynapticPump x = error (show x)

    convertTimeConstant (Value.Application (Value.Alternative altName) (Value.Record fields)) | isAlt altName =
      Value.Record ( HashMap.insert "type" (Value.Scalar (Text altName)) fields )
    convertTimeConstant x = error $ "Encountered invalid timeConstant: " ++ show x
    isAlt :: Text.Text -> Bool
    isAlt alt = alt == "Sigmoid" || alt ==  "LinearExp" || alt == "Instantaneous"

    convertTransmitterPumpParams
        (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                       [("target_concentration", targetConcentration)
                       ,("time_constant", timeConstant)
                       ])) = Value.Record $ HashMap.fromList
                             [("target_concentration", targetConcentration)
                             ,("time_constant", convertTimeConstant timeConstant)]
    convertTransmitterPumpParams x = error (show x)

    convertPostsynapticReceptor
        (Value.Record ( List.sortBy (Ord.comparing fst) . HashMap.toList ->
                        [("membrane_channel", membraneChannel)
                        ,("neurotransmitter_sensitivity", neurotransmitterSensitivity)])) =
          Value.Record $ HashMap.fromList
            [("membrane_channel", convertMembraneChannel membraneChannel)
            ,("neurotransmitter_sensitivity", convertNeurotransmitterSensitivity neurotransmitterSensitivity)]
    convertPostsynapticReceptor x = error (show x) -- TODO "Error"

    -- TODO: Factor this out?
    convertMembraneChannel :: Value -> Value
    convertMembraneChannel
        (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                        [("channel", channel)
                        ,("siemens_per_square_cm", conductance)
                        ])) = Value.Record
                                [("channel", channel)
                                ,("siemens_per_square_cm", conductance)]
    convertMembraneChannel x = error $ "Encountered unexpected membraneChanne: " ++ show x
    convertNeurotransmitterSensitivity
        (Value.Record (List.sortBy (Ord.comparing fst) . HashMap.toList ->
         [("concentration_at_half_max_molar", concentrationAtHalfMaxMolar)
         ,("slope", slope)
         ,("transmitter", transmitter)
         ])) = Value.Record $ HashMap.fromList
                                [("transmitter", convertTransmitter transmitter)
                                ,("concentration_at_half_max_molar", concentrationAtHalfMaxMolar)
                                ,("slope", slope)]
    convertNeurotransmitterSensitivity x = error (show x)
    convertTransmitter (Value.Application (Value.Alternative altName) _ ) =
      Value.Scalar (Text altName)
    convertTransmitter x = error (show x)
apply (Value.Builtin Synapse) x = error (show x)


apply (Value.Builtin IntegerEven) (Value.Scalar x)
    | Just n <- asInteger x = Value.Scalar (Bool (even n))
apply (Value.Builtin IntegerOdd) (Value.Scalar x)
    | Just n <- asInteger x = Value.Scalar (Bool (odd n))
apply
    (Value.Application (Value.Builtin RealEqual) (Value.Scalar l))
    (Value.Scalar r)
    | Just m <- asReal l
    , Just n <- asReal r =
        Value.Scalar (Bool (m == n))
apply
    (Value.Application (Value.Builtin RealLessThan) (Value.Scalar l))
    (Value.Scalar r)
    | Just m <- asReal l
    , Just n <- asReal r =
        Value.Scalar (Bool (m < n))
apply (Value.Builtin IntegerAbs) (Value.Scalar x)
    | Just n <- asInteger x = Value.Scalar (Natural (fromInteger (abs n)))
apply (Value.Builtin RealNegate) (Value.Scalar x)
    | Just n <- asReal x = Value.Scalar (Real (negate n))
apply (Value.Builtin IntegerNegate) (Value.Scalar x)
    | Just n <- asInteger x = Value.Scalar (Integer (negate n))
apply (Value.Builtin IntegerToReal) (Value.Scalar x)
    | Just n <- asInteger x = Value.Scalar (Real (fromIntegral n))
apply (Value.Builtin RealShow) (Value.Scalar (Natural n)) =
    Value.Scalar (Text (Text.pack (show n)))
apply (Value.Builtin RealShow) (Value.Scalar (Integer n)) =
    Value.Scalar (Text (Text.pack (show n)))
apply (Value.Builtin RealShow) (Value.Scalar (Real n)) =
    Value.Scalar (Text (Text.pack (show n)))
apply (Value.Builtin RealSin) (Value.Scalar (Real n)) =
    Value.Scalar (Real (fromFloatDigits @Double (roundTo12Places $ sin (toRealFloat n))))
apply (Value.Builtin RealCos) (Value.Scalar (Real n)) =
    Value.Scalar (Real (fromFloatDigits @Double (roundTo12Places $ cos (toRealFloat n))))
apply
    (Value.Application (Value.Builtin TextEqual) (Value.Scalar (Text l)))
    (Value.Scalar (Text r)) =
        Value.Scalar (Bool (l == r))
apply
    (Value.Application
        (Value.Builtin JSONFold)
        (Value.Record
            (List.sortBy (Ord.comparing fst) . HashMap.toList ->
                [ ("array"  , arrayHandler )
                , ("bool"   , boolHandler  )
                , ("integer", integerHandler)
                , ("natural", naturalHandler)
                , ("null"   , nullHandler   )
                , ("object" , objectHandler )
                , ("real"   , realHandler  )
                , ("string" , stringHandler )
                ]
            )
        )
    )
    v0 = loop v0
  where
    loop (Value.Scalar (Bool b)) =
        apply boolHandler (Value.Scalar (Bool b))
    loop (Value.Scalar (Natural n)) =
        apply naturalHandler (Value.Scalar (Natural n))
    loop (Value.Scalar (Integer n)) =
        apply integerHandler (Value.Scalar (Integer n))
    loop (Value.Scalar (Real n)) =
        apply realHandler (Value.Scalar (Real n))
    loop (Value.Scalar (Text t)) =
        apply stringHandler (Value.Scalar (Text t))
    loop (Value.Scalar Null) =
        nullHandler
    loop (Value.List elements) =
        apply arrayHandler (Value.List (fmap loop elements))
    loop (Value.Record keyValues) =
        apply objectHandler (Value.List (Seq.fromList (map adapt (HashMap.toList keyValues))))
      where
        adapt (key, value) =
            Value.Record
                [("key", Value.Scalar (Text key)), ("value", loop value)]
    loop v =
        v
apply function argument =
  -- error $ "not covered: " ++ show function ++ " " ++ show argument
  Value.Application function argument

countNames :: Text -> [Text] -> Int
countNames name = length . filter (== name)

{-| Obtain a unique variable, given a list of variable names currently in scope

    >>> fresh "x" [ "x", "y", "x" ]
    Variable "x" 2
    >>> fresh "y" [ "x", "y", "x" ]
    Variable "y" 1
    >>> fresh "z" [ "x", "y", "x" ]
    Variable "z" 0
-}
fresh
    :: Text
    -- ^ Variable base name (without the index)
    -> [Text]
    -- ^ Variables currently in scope
    -> Value
    -- ^ Unique variable (including the index)
fresh name names = Value.Variable name (countNames name names)

-- | Convert a `Value` back into the surface `Syntax`
quote
    :: [Text]
    -- ^ Variable names currently in scope (starting at @[]@ for a top-level
    --   expression)
    -> Value
    -> Syntax () Void
quote names value =
    case value of
        Value.Variable name index ->
            Syntax.Variable{ index = countNames name names - index - 1, .. }

        Value.Lambda closure@(Closure name _ _) ->
            Syntax.Lambda{ nameLocation = (), .. }
          where
            variable = fresh name names

            body = quote (name : names) (instantiate closure variable)

        Value.Application function argument ->
            Syntax.Application
                { function = quote names function
                , argument = quote names argument
                , ..
                }

        Value.List elements ->
            Syntax.List{ elements = fmap (quote names) elements, .. }

        Value.Record fieldValues ->
            Syntax.Record
                { fieldValues = map adapt (HashMap.toList fieldValues)
                , ..
                }
          where
            adapt (field, value_) = (field, quote names value_)

        Value.Field record field ->
            Syntax.Field{ record = quote names record, fieldLocation = (), .. }

        Value.Alternative name ->
            Syntax.Alternative{..}

        Value.Merge handlers ->
            Syntax.Merge{ handlers = quote names handlers, .. }

        Value.If predicate ifTrue ifFalse ->
            Syntax.If
                { predicate = quote names predicate
                , ifTrue = quote names ifTrue
                , ifFalse = quote names ifFalse
                , ..
                }

        Value.Scalar scalar ->
            Syntax.Scalar{..}

        Value.Operator left operator right ->
            Syntax.Operator
                { left = quote names left
                , operatorLocation = ()
                , right = quote names right
                , ..
                }

        Value.Builtin builtin ->
            Syntax.Builtin{..}
        Value.Channel inner ->
            quote names inner
        Value.Membrane inner ->
            quote names inner
        Value.Neuron inner ->
            quote names inner
        Value.Stimulator inner ->
            quote names inner
        Value.Scene inner ->
            quote names inner
        Value.Synapse inner ->
            quote names inner
  where
    location = ()

roundTo12Places :: Double -> Double
roundTo12Places x = realToFrac (round (x * 1e20) :: Integer) * 1e-20
