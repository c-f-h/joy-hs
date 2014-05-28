module Primitives(primitives) where

import Interpreter
import qualified StackManip

import Data.Time.Clock.POSIX
import Debug.Trace


-- Primitives

pop (x:xs) = xs
pop _      = error "pop: stack underflow"

dup (x:xs) = x:x:xs
dup _      = error "dup: stack empty"

dip vocab ((Quot q):x:xs) = do s' <- runQuotation q vocab xs; return (x:s')
dip _ _                   = error "dip: value and quotation expected"

cons ((Quot q):x:xs) = Quot (x:q) : xs
cons _               = error "cons: value and quotation expected"

uncons ((Quot (x:is)):xs) = (Quot is):x:xs
uncons _ = error "uncons: quotation with at least one element expected"

append ((Quot r):(Quot q):xs) = (Quot (q ++ r)):xs
append _ = error "append: two quotations expected"

printVal _ (x:xs) = do putStrLn (format x); return xs
printVal _ _      = error "print: stack empty"

ifThenElse vocab ((Quot qelse):(Quot qthen):(Quot qif):xs) =
    do (result:_) <- runQuotation qif vocab xs
       if (isTrue result) then
           runQuotation qthen vocab xs
         else
           runQuotation qelse vocab xs
ifThenElse _ _ = error "ifte: three quotations expected"

arith op ((Number y):(Number x):xs) = Number(op x y):xs
arith _ _ = error "arithmetic operation: two numbers expected"

comparison op ((Number y):(Number x):xs) = toTruth(op x y):xs
comparison _ _ = error "comparison operation: two numbers expected"

logic op (y:x:xs) = toTruth (op (isTrue x) (isTrue y)) : xs
logic _ _ = error "logic operation: two values expected"

lnot (x:xs) = toTruth (not (isTrue x)) : xs
lnot _      = error "null: value expected"

debugDumpStack _ s = do dumpStack s ; return s

stack xs = (Quot xs):xs
unstack ((Quot ys):xs) = ys
unstack _ = error "unstack: quotation expected"

truncMod x y = fromInteger ((truncate x) `mod` (truncate y)) :: Double

time _ s = do
    time <- getPOSIXTime
    return $ Number (realToFrac time :: Double) : s



primitives =
    [ ("pop",    Primitive pop)
    , ("dup",    Primitive dup)
    , ("dip",    EnvPrimitive dip)
    , ("cons",   Primitive cons)
    , ("uncons", Primitive uncons)
    , ("append", Primitive append)
    , (".",      EnvPrimitive printVal)
    , ("ifte",   EnvPrimitive ifThenElse)
    , ("+",      Primitive $ arith (+))
    , ("-",      Primitive $ arith (-))
    , ("*",      Primitive $ arith (*))
    , ("/",      Primitive $ arith (/))
    , ("%",      Primitive $ arith truncMod)
    , ("=",      Primitive $ comparison (==))
    , ("<=",     Primitive $ comparison (<=))
    , (">=",     Primitive $ comparison (>=))
    , ("<",      Primitive $ comparison (<))
    , (">",      Primitive $ comparison (>))
    , ("and",    Primitive $ logic (&&))
    , ("or",     Primitive $ logic (||))
    , ("null",   Primitive lnot)
    , ("stack",  Primitive stack)
    , ("unstack",Primitive unstack)
    , ("???",    EnvPrimitive debugDumpStack)
    , ("time",   EnvPrimitive time)
    ]
