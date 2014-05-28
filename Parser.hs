module Parser where

import Interpreter
import Text.ParserCombinators.Parsec
import Control.Monad
import Control.Applicative hiding ((<|>),many)

instance Applicative (GenParser s a) where
    pure  = return
    (<*>) = ap


--p `cat` q = do v <- p; w <- q; return (v ++ w)
p `cat` q = (++) <$> p <*> q

number = do
    str <- option "" (string "-")
           `cat` (many1 digit)
           `cat` option "" (string "." `cat` many1 digit)
    spaces
    return $ Number (read str :: Double)
  <?> "number"

wordLetter = letter <|> digit <|> oneOf "+-*/<>=!?§$%&@~#´`',:."

word = do
    w <- many1 wordLetter
    spaces
    if w == "DEFINE" then
        fail "invalid occurrence of DEFINE"
      else
        return (Symbol w)
  <?> "word"

instruction = quotation <|> (try number) <|> word

nakedQuotation = many instruction

quotation = do
    char '['; spaces
    quot <- nakedQuotation
    char ']'; spaces
    return (Quot quot)
  <?> "quotation"

definitionHeader = do
    name <- word
    string "=="; space; spaces
    case name of
        Symbol x -> return x
        _ -> fail "invalid name for definition"

definition = do
    string "DEFINE"; space; spaces
    name <- definitionHeader
    quot <- nakedQuotation
    char ';' ; spaces
    return (name, Quotation quot)

program = do
    spaces
    vocab <- many definition
    quot <- nakedQuotation
    eof
    return (vocab,quot)
