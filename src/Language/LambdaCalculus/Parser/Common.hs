module Language.LambdaCalculus.Parser.Common
  ( infoFrom
  , parens
  , identifier
  , reserved
  , reservedOp
  , whiteSpace
  ) where

import Language.LambdaCalculus.AST

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

infoFrom :: SourcePos -> Info
infoFrom pos = Info (sourceLine pos) (sourceColumn pos)

lcDef :: LanguageDef st
lcDef = emptyDef
  { P.identStart      = letter
  , P.identLetter     = letter <|> char '\''
  , P.reservedOpNames = [".", "\\"]
  }

lexer       = P.makeTokenParser lcDef
parens      = P.parens lexer
identifier  = P.identifier lexer
reserved    = P.reserved lexer
reservedOp  = P.reservedOp lexer
whiteSpace  = P.whiteSpace lexer
