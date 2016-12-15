-- |
-- The main module
--
module Language.LambdaCalculus
  ( module LC
  , run
  ) where

import Language.LambdaCalculus.AST as LC
import Language.LambdaCalculus.Context as LC
import Language.LambdaCalculus.Parser as LC
import Language.LambdaCalculus.Evaluator as LC
import Language.LambdaCalculus.Pretty as LC

run :: String -> Either String String
run script =
   case parseLC script of
     Left e -> Left (show e)
     Right p -> Right (printTm [] (eval [] p))
