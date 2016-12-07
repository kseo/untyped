module Language.LambdaCalculus.Parser.Term
  ( parseLC
  ) where

-- | See http://mattwetmore.me/posts/parsing-combinators-with-parser-combinators.html
import Data.List (elemIndex)

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Context
import Language.LambdaCalculus.Parser.Common

import Text.Parsec
import Text.Parsec.Combinator (between, sepBy1, chainr1)

type BoundContext = [String]
type LCParser = Parsec String BoundContext Term

parseAbs :: LCParser -> LCParser
parseAbs termParser = do
  reservedOp "\\"
  v <- identifier
  modifyState (v :)
  reservedOp "."
  term <- termParser
  modifyState tail
  pos <- getPosition
  return $ TmAbs (infoFrom pos) v term

parseVar :: LCParser
parseVar = do
  v <- identifier
  list <- getState
  findVar v list

findVar :: String -> BoundContext -> LCParser
findVar v list = case elemIndex v list of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n  -> do
    pos <- getPosition
    return $ TmVar (infoFrom pos) n (length list)

parseNonApp :: LCParser
parseNonApp =  parens parseTerm   -- (M)
           <|> parseAbs parseTerm -- $\lambda$x.M
           <|> parseVar           -- x

parseTerm :: LCParser
parseTerm = chainl1 parseNonApp $ do
  whiteSpace
  pos <- getPosition
  return $ TmApp (infoFrom pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped lambda-calculus"

parseLC :: String -> Either ParseError Term
parseLC = parseWith (whiteSpace >> parseTerm)
