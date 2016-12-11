module Language.LambdaCalculus.Pretty.Term
  ( printTm
  ) where

import Control.Arrow ((<+>))
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
import qualified Control.Arrow as A

import Data.Maybe (fromMaybe)

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Context

data NamedTerm =
    NVar String
  | NAbs String NamedTerm
  | NApp NamedTerm NamedTerm
  deriving (Show)

assignName :: Context -> Term -> NamedTerm
assignName ctx = go
  where
    go (TmAbs _ x t1) =
      let (ctx', x') = pickFreshName ctx x
      in NAbs x' (assignName ctx' t1)
    go (TmApp _ t1 t2) = NApp (assignName ctx t1) (assignName ctx t2)
    go (TmVar _ x n) =
      if ctxLength ctx == n then
        NVar (indexToName ctx x)
      else
        error "[bad index]"

var :: Pattern () NamedTerm String
var = mkPattern var'
  where
  var' (NVar s) = Just s
  var' _ = Nothing

lam :: Pattern () NamedTerm (String, NamedTerm)
lam = mkPattern abs'
  where abs' (NAbs s e) = Just (s, e)
        abs' _ = Nothing

app :: Pattern () NamedTerm (NamedTerm, NamedTerm)
app = mkPattern app'
  where app' (NApp e1 e2) = Just (e1, e2)
        app' _ = Nothing

parenthesize :: Pattern () NamedTerm String -> Pattern () NamedTerm String
parenthesize = fmap parens
  where
  parens s = '(':s ++ ")"

term :: Pattern () NamedTerm String
term = buildPrettyPrinter ops (var <+> parenthesize term)
  where
    ops = OperatorTable
      [ [ AssocL app $ \e1 e2 -> e1 ++ " " ++ e2 ]
      , [ Wrap lam $ \b s -> "\\" ++ b ++ "." ++ s ]
      ]

printTm :: Context -> Term -> String
printTm ctx = fromMaybe (error "Incomplete pattern") . (flip evalStateT () . printTm') . assignName ctx

printTm' :: NamedTerm -> StateT () Maybe String
printTm' = A.runKleisli $ runPattern term
