module Main where

import Parser
import Text.ParserCombinators.Parsec(parseFromFile)
import Interpreter
import Primitives
import Optimizer
import qualified Data.Map as Map


must :: (Show a) => Either a b -> b
must = either (error . show) id

runJoy :: String -> IO(Stack)
runJoy fname = do
    (vocab,quot) <- fmap must $ parseFromFile program fname
    runQuotation quot (Map.fromList $ optimizeVocabulary $ primitives ++ vocab) []

main = do
    s <- runJoy "test.joy"
    if null s then 
        return ()
      else
        do putStrLn "Residual stack (top to bottom):"
           dumpStack s
