-- | Extra parser combinators for patterns which are used frequently.
module Ear.Parser.Util (
    pad
  , skip
  , sepBy'
  , endBy'
  , sepEndBy'
) where

import Text.Parsec hiding (many, optional, (<|>))
import Control.Applicative

-- |  Allows  a parser  to  consume  any  spaces or  tabs  surrounding
-- it. Available for parsers using a stream of [Char] only.
pad :: (Monad m)
    => ParsecT [Char] u m a
    -> ParsecT [Char] u m a

pad p = s *> p <* s
  where s = many $ oneOf " \t"

-- | Matches a parser, and if successful returns ().
skip :: (Stream s m t)
     => ParsecT s u m a
     -> ParsecT s u m ()

skip p = () <$ p

-- | A version of 'sepBy' which uses 'try' internally to avoid failure
-- when the  separator has  been matched but  the parser  following it
-- does  not.   In  that case,  it  will  simply  return what  it  has
-- successfully parsed  up to  that point and  leave the  failing area
-- (separator and rest) unconsumed.
sepBy' :: (Stream s m t)
       => ParsecT s u m a   -- ^ The parser to enumerate.
       -> ParsecT s u m sep -- ^ The separator to be matched between each item.
       -> ParsecT s u m [a] -- ^ The list of return values for each match.

p `sepBy'` sep = (:) <$> p <*> many (try $ sep *> p) <|> pure []

-- | A  version of  'endBy' which uses  'try' internally. It  does not
-- fail when  there is  no separator after  a parsed item,  rather, it
-- pretends it  hasn't consumed that item  at all and  returns what it
-- has normally.
endBy' :: (Stream s m t)
       => ParsecT s u m a   -- ^ The parser to enumerate.
       -> ParsecT s u m sep -- ^ The separator to be matched between each item.
       -> ParsecT s u m [a] -- ^ The list of return values for each match.

p `endBy'` sep = many $ try $ p <* sep

sepEndBy' :: (Stream s m t)
          => ParsecT s u m a   -- ^ The parser to enumerate.
          -> ParsecT s u m sep -- ^ The separator to be matched between each item.
          -> ParsecT s u m [a] -- ^ The list of return values for each match.

p `sepEndBy'` sep = p `sepBy'` sep <* optional (try sep)