module Interpreter where

import qualified Data.Map as Map
import StackManip


data Value = Symbol String | Number Double | Quot [Value]
    deriving (Eq,Show)

type Stack = [Value]

data Word = Quotation [Value]                 -- composite word
            | Primitive (Stack -> Stack)      -- simple, pure stack effect
            | EnvPrimitive (Vocabulary -> Stack -> IO(Stack))
            | StackEffect Swizzle

--type Vocabulary = [(String, Word)]
type Vocabulary = Map.Map String Word

getWord :: String -> Vocabulary -> Word
getWord w vocab =
    maybe (error $ "undefined word " ++ w) id (Map.lookup w vocab)


isTrue (Number x) = x /= 0.0
isTrue (Quot q)   = not (null q)

toTruth :: Bool -> Value
toTruth b = if b then Number 1.0 else Number 0.0

-- run the given word, whether given as a quotation or a primitive
runWord w env stack = case w of
    Quotation q     -> runQuotation q env stack
    Primitive f     -> return $ f stack 
    EnvPrimitive f  -> f env stack 
    StackEffect s   -> return $ applySwizzle s stack

runInstruction ins env stack = case ins of
    Symbol w    -> runWord (getWord w env) env stack
    x           -> return (x:stack)

runQuotation :: [Value] -> Vocabulary -> Stack -> IO(Stack)
runQuotation [] _ s = return s
runQuotation (i:is) env s =
    do s' <- runInstruction i env s
       runQuotation is env s'

quotCons :: Value -> Value -> Value
quotCons x (Quot q) = Quot (x:q)
quotCons _ _        = error "Error in cons, second argument not a quotation"


-- stack swizzling

computePosition :: Stack -> StackPos -> Value
computePosition stack (Nil)        = Quot []
computePosition stack (From i)     = stack !! i
computePosition stack (Cons s1 ss) =
    quotCons (computePosition stack s1) (computePosition stack ss)
computePosition stack _ =
    error "Attempt to apply ill-defined stack effect."

applySwizzle :: Swizzle -> Stack -> Stack
applySwizzle s@(Swizzle i (Just o) ef) stack =
    newtop ++ bottom where
        (top,bottom) = splitAt i stack
        newtop = map (computePosition top) ef
applySwizzle _ _ =
    error "Attempt to apply ill-defined stack effect."



-- pretty-print stack
formatStack = unlines . map format

dumpStack s = putStrLn (formatStack s)

-- pretty-print values
format (Symbol s) = s
format (Number x) = if (isInteger x) then show (truncate x) else show x where
                        isInteger x = snd (properFraction x) == 0
format (Quot [])  = "[]"
format (Quot q)   = concat ["[ ", unwords $ map format q, " ]"]

