module StackManip where

import Data.List
import Control.Applicative

{-
 - An algebra of stack manipulations.
 -
 - A stack manipulation is described by number of inputs,
 - number of outputs (if known), and a list of output
 - positions which describe which original position every
 - output comes from.
 -
 - Note that of the number of outputs is Nothing, the
 - stack effect includes some unquote operation whose
 - number of results is unknown. By composing such an
 - effect with a well-defined one from the left and
 - simplifying, a well-defined result can sometimes
 - be obtained.
 -}

{- A StackPos describes the origin of a stack position
 - after some stack transformation.
 -}
data StackPos = Cons StackPos StackPos
              | Nil
              | Unquote StackPos
              | From Int
              deriving (Eq,Show)

isSimple (Unquote _) = False
isSimple _           = True

data Swizzle = Swizzle Int (Maybe Int) [StackPos]
    deriving (Eq,Show)

isWellDefined (Swizzle _ (Just _) _) = True
isWellDefined _                      = False

pop  = Swizzle 1 (Just 0) []
dup  = Swizzle 1 (Just 2) [From 0, From 0]
dip  = Swizzle 2 Nothing  [From 1, Unquote (From 0)]
cons = Swizzle 2 (Just 1) [Cons (From 1) (From 0)]
nil  = Swizzle 0 (Just 1) [Nil]

eval = Swizzle 1 Nothing  [Unquote (From 0)]


composeStack :: (Int -> StackPos) -> StackPos -> StackPos
composeStack f (Cons s0 ss) = Cons (composeStack f s0) (composeStack f ss)
composeStack _ Nil          = Nil
composeStack f (Unquote sp) = Unquote (composeStack f sp)
composeStack f (From i)     = f i

--toList Nil          = []
--toList (Cons s0 ss) = s0 : (toList ss)
--toList _            = error "Malformed list"

toSimpleList :: StackPos -> Maybe [StackPos]
toSimpleList Nil          = Just []
toSimpleList (Cons s0 ss)
        | isSimple s0     = (:) <$> (Just s0) <*> (toSimpleList ss)
        | otherwise       = Nothing
toSimpleList _            = Nothing


simplify :: StackPos -> Maybe [StackPos]
simplify (Unquote l)   = fmap reverse $ toSimpleList l
simplify x             = Just [x]

trySimplify :: [StackPos] -> Maybe [StackPos]
trySimplify = fmap concat . mapM simplify

maxIndex (Cons s0 ss) = max (maxIndex s0) (maxIndex ss)
maxIndex Nil          = -1
maxIndex (Unquote sp) = maxIndex sp
maxIndex (From i)     = i

maxIndexFromList ss = foldl' max (-1) (map maxIndex ss)

-- Returns a function which maps new stack indices to original
-- stack positions according to the given stack manipulation.
toFunction :: Swizzle -> Int -> StackPos
toFunction (Swizzle i (Just o) ef) n
    |  n < o     = ef !! n
    |  otherwise = From (n + i - o)
toFunction _ _ = error "Stack effect has no well-defined output size."

-- Computes the effect of executing first the stack manipulation
-- word s1, then s2, if the result is well-defined.
compose :: Swizzle -> Swizzle -> Maybe Swizzle
compose s1@(Swizzle i1 (Just o1) ef1) s2@(Swizzle i2 (Just o2) ef2) =
    Just $ normalize $ Swizzle i3 (Just o3) ef3 where
        g n = composeStack (toFunction s1) (toFunction s2 n)
        o3 = o2 + max 0 (o1 - i2)
        i3 = (o3 + i2 - o2) + i1 - o1
        ef3 = map g [0..(o3-1)]
compose s1@(Swizzle i1 (Just o1) ef1) (Swizzle i2 o2 ef2) =
    let g = composeStack (toFunction s1)
        concreteEffects = map g ef2 in
        do ef2' <- trySimplify concreteEffects
           let o2 = length ef2'
               o3 = o2 + max 0 (o1 - i2)
               i3 = (o3 + i2 - o2) + i1 - o1 in
             Just $ normalize $ Swizzle i3 (Just o3) ef2'
compose _ _ = Nothing



normalize s@(Swizzle i (Just o) ef) =
    if o > 0
       && (ef !! (o-1) == From(i-1))
       && (maxIndexFromList (init ef) < i-1) then
        normalize $ Swizzle (i-1) (Just (o-1)) (init ef)
    else s
normalize s = s


reduce (s1:s2:ss) =
    case compose s1 s2 of
        Just s  -> reduce (s:ss)
        Nothing -> s1:(reduce (s2:ss))
reduce ss = ss
