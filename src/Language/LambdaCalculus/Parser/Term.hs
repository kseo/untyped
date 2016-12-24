{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Language.LambdaCalculus.Parser.Term
  ( parseLC
  ) where

-- | See http://mattwetmore.me/posts/parsing-combinators-with-parser-combinators.html
import Data.List (elemIndex)

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Parser.Common

import Text.Parsec

parseAbs :: LCParser Term
parseAbs = do
  pos <- getPosition
  backslash
  v <- identifier
  modifyState (v :)
  dot
  term <- parseTerm
  modifyState tail
  return $ TmAbs (infoFrom pos) v term

parseVar :: LCParser Term
parseVar = do
  v <- identifier
  list <- getState
  findVar v list

findVar :: String -> BoundContext -> LCParser Term
findVar v list = case elemIndex v list of
  Nothing -> fail $ "The variable " ++ v ++ " has not been bound"
  Just n  -> do
    pos <- getPosition
    return $ TmVar (infoFrom pos) n (length list)

parseNonApp :: LCParser Term
parseNonApp =  parens parseTerm   -- (M)
           <|> parseAbs           -- $\lambda$x.M
           <|> parseVar           -- x

parseTerm :: LCParser Term
parseTerm = chainl1 parseNonApp $ do
  whiteSpace
  pos <- getPosition
  return $ TmApp (infoFrom pos)

parseWith :: Parsec String [u] a -> String -> Either ParseError a
parseWith p = runParser p [] "untyped lambda-calculus"

parseLC :: String -> Either ParseError Term
parseLC = parseWith (whiteSpace >> parseTerm)
