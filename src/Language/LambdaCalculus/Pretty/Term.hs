module Language.LambdaCalculus.Pretty.Term
  ( printTm
  ) where

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Context

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
