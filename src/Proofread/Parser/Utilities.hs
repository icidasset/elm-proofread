module Proofread.Parser.Utilities where

import Control.Applicative (Alternative, (<|>))
import Prelude hiding (or)
import Proofread.Parser.Types
import Text.Megaparsec.Char

import qualified Control.Applicative


-- ⚗️ Combinators


and :: (Applicative f, Monoid a) => f a -> f a -> f a
and =
    Control.Applicative.liftA2 mappend


andThen :: Monad m => m a -> m b -> m b
andThen =
    (>>)


or :: Alternative f => f a -> f a -> f a
or =
    (<|>)


one :: a -> a
one =
    id


maybeSome :: Alternative f => f a -> f [a]
maybeSome =
    Control.Applicative.many



-- 🤖 Predefined combinations


spaceCharacter :: Parser Char
spaceCharacter =
    char ' '


whitespace :: Parser Char
whitespace =
    spaceChar
