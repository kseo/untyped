module Language.LambdaCalculus.Pretty.Term
  ( printTm
  ) where

import Language.LambdaCalculus.AST

data Binding = NameBind deriving (Show)

type Context = [(String, Binding)]

ctxLength :: Context -> Int
ctxLength = length

indexToName :: Context -> Int -> String
indexToName ctx n = fst $ ctx !! n

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x
  | x `elem` map fst ctx = pickFreshName ctx $ x ++ "'"
  | otherwise = ((x, NameBind) : ctx , x)

printTm :: Context -> Term -> String
printTm ctx t = case t of
  TmAbs _ x t1 -> let
      (ctx', x') = pickFreshName ctx x
    in "(\\" ++ x' ++ "." ++ printTm ctx' t1 ++ ")"
  TmApp _ t1 t2 ->
    "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
  TmVar _ x n ->
    if ctxLength ctx == n then
      indexToName ctx x
    else
      "[bad index]"
