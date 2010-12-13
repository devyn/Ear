module Ear.Parser (
  -- * Parse Tree
    Rule(..)
  , Pattern
  , Part(..)
    
  -- * Invoking the Parser
  , parse
) where

import Prelude hiding (lines)
import Text.Parsec hiding (optional, parse, many, (<|>))
import qualified Text.Parsec as P
import Control.Applicative

--- Parse Tree ---

-- | An Ear rule, like:
-- 
-- > A == B | A + B = A * 2
-- 
-- Which parses as:
-- 
-- @
--     'Rule' [['Variable' \"A\", 'Name' \"==\", 'Variable' \"B\"]]
--          [['Variable' \"A\", 'Name' \"+\",  'Variable' \"B\"]
--           ['Variable' \"A\", 'Name' \"*\",  'PrimN' 2]]
-- @
-- 
-- The first field is for guards, the second is for the patterns in
-- the equation.

data Rule = Rule [Pattern] [Pattern]
          deriving (Show, Eq)

-- | A pattern is simply a list of 'Part's.

type Pattern = [Part]

-- | 'Part's are simply the components of 'Pattern's.

data Part = Variable [Char] -- ^ A variable, such as @Var@.
          | Name [Char]     -- ^ A name, such as @name@.
          | Inner Pattern   -- ^ An inner pattern, usually enclosed by parentheses.
          | PrimN Float     -- ^ A primitive number.
          | PrimS String    -- ^ A primitive string.
          deriving (Show, Eq)

-- | Run the parser.

parse :: SourceName               -- ^ The source of that which is
                                  -- being parsed. Often a file name
                                  -- or URI..
      -> String                   -- ^ The string of characters to be
                                  -- parsed.
      -> Either ParseError [Rule] -- ^ If successful, a list of rules
                                  -- on the 'Right'. Otherwise, a
                                  -- 'ParseError' on the 'Left'.

parse  = P.parse earDoc

--- internal parser, not exported ---

type Parser = Parsec [Char] ()

earDoc     :: Parser [Rule]
rule       :: Parser Rule
guards     :: Parser [Pattern]
equation   :: Parser [Pattern]
part       :: Parser Part
pattern    :: Parser Pattern
primN      :: Parser Part
primS      :: Parser Part
inner      :: Parser Part
variable   :: Parser Part
name       :: Parser Part
pad        :: Parser a -> Parser a
skip       :: Parser a -> Parser ()
lines      :: Parser [Rule]
comment    :: Parser ()
white      :: Parser ()
sepBy'     :: Parser a -> Parser b -> Parser [a] -- uses 'try'
endBy'     :: Parser a -> Parser b -> Parser [a] -- uses 'try'
sepEndBy'  :: Parser a -> Parser b -> Parser [a] -- uses 'try'

earDoc = white *> lines <* eof

rule = Rule <$> guards <*> equation

guards = pad pattern `endBy'` char '|'

equation = do ps <- pad pattern `sepBy'` char '='
              if length ps > 1
                then return ps
                else fail "A rule must contain at least two equivalent patterns."

part =     try primN
       <|> try primS 
       <|> try inner
       <|> try variable
       <|> try name

pattern = part `sepBy'` some (oneOf " \t")

primN = empty

primS = empty

inner = Inner <$> between (char '(') (char ')') (pad pattern)

variable = Variable <$> ((:) <$> oneOf ['A'..'Z'] <*> many (noneOf " \t\n()=|"))

name = Name <$> some (noneOf " \t\n()=|")

pad p = s *> p <* s
  where s = many $ oneOf " \t"

skip p = () <$ p

lines = (pad rule <* optional comment) `sepEndBy` (newline *> white)

comment = skip $ char '#' *> many (noneOf "\n")

white = skip $ many (skip space <|> comment)

p `sepBy'` sep = (:) <$> p <*> many (try $ sep *> p) <|> return []

p `endBy'` sep = many $ try $ p <* sep

p `sepEndBy'` sep = p `sepBy'` sep <* optional (try sep)
