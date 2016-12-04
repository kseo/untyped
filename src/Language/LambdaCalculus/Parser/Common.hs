module Language.LambdaCalculus.Parser.Common
  ( infoFrom
  ) where

import Language.LambdaCalculus.AST
import Text.Parsec

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)
