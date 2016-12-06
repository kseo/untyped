module Language.LambdaCalculus.Evaluator
  ( eval
  ) where

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Context

termShift :: Int -> Term -> Term
termShift d = walk 0
  where walk c t = case t of
                      (TmAbs fi x t1)   -> TmAbs fi x (walk (c + 1) t1)
                      (TmApp fi t1 t2)  -> TmApp fi (walk c t1) (walk c t2)
                      (TmVar fi x n)    -> if x >= c then TmVar fi (x + d) (n + d)
                                                     else TmVar fi x (n + d)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
    where walk c t = case t of
                      (TmAbs fi x t1)   -> TmAbs fi x (walk (c + 1) t1)
                      (TmApp fi t1 t2)  -> TmApp fi (walk c t1) (walk c t2)
                      (TmVar fi x n)    -> if x == j + c then termShift c s
                                                         else t
termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

isVal :: Context -> Term -> Bool
isVal _ TmAbs{} = True
isVal _ _ = False

eval1 :: Context -> Term -> Maybe Term
eval1 ctx t = case t of
                (TmApp _ (TmAbs _ x t12) v2) | isVal ctx v2 ->
                  return $ termSubstTop v2 t12
                (TmApp fi v1 t2) | isVal ctx v1 -> do
                  t2' <- eval1 ctx t2
                  return $ TmApp fi v1 t2'
                (TmApp fi t1 t2) -> do
                  t1' <- eval1 ctx t1
                  return $ TmApp fi t1' t2
                _ -> Nothing

eval :: Context -> Term -> Term
eval ctx t = case eval1 ctx t of
               Nothing -> t
               Just t' -> eval ctx t'
