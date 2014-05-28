module Optimizer where

import Interpreter
import qualified StackManip
import StackManip(Swizzle)
import qualified Data.Map as Map

type EffectMap = Map.Map String [Swizzle]

knownEffects :: EffectMap
knownEffects = Map.fromList [
      ( "pop",  [StackManip.pop] )
    , ( "dup",  [StackManip.dup] )
    , ( "dip",  [StackManip.dip] )
    , ( "cons", [StackManip.cons] )
    ]

getStackEffect :: EffectMap -> Value -> Maybe [Swizzle]
getStackEffect effects word = case word of
    Symbol s  -> Map.lookup s effects
    Quot []   -> Just [StackManip.nil]
    -- TODO: would be good to be able to analyze literal quotations. how? this is not the way
    --Quot q    -> do ef <- analyzeQuotation effects q
    --                return $ StackManip.reduce
    --                       $ ef ++ [StackManip.nil] ++ (replicate (length ef) StackManip.cons)
    _         -> Nothing

analyzeQuotation effects =
    fmap (StackManip.reduce . concat) . mapM (getStackEffect effects)

optimizeWord :: (String,Word) -> EffectMap -> ((String,Word), EffectMap)
optimizeWord (name, Quotation q) effects =
    case (analyzeQuotation effects q) of
        Just [swizzle] | StackManip.isWellDefined swizzle -> (wordentry, neweffects) where
            wordentry = (name, StackEffect swizzle)
            neweffects = Map.insert name [swizzle] effects
        Just swizzles -> ((name, Quotation q), neweffects) where
            neweffects = Map.insert name swizzles effects
        Nothing -> ((name, Quotation q), effects)
optimizeWord w effects = (w,effects)

optimizeVocabulary :: [(String,Word)] -> [(String,Word)]
optimizeVocabulary words = opt knownEffects words where
    opt effects [] = []
    opt effects (word:words) =
        let (optword, effects') = optimizeWord word effects in
            optword : (opt effects' words)

