module Language.LambdaCalculus.Context
  ( Binding(..)
  , Context
  , ctxLength
  , indexToName
  , pickFreshName
  ) where

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
