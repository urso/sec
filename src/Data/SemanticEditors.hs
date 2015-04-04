
module Data.SemanticEditors(result, first, second, each, editIf, set, argument, 
                            left, right, ioref, maybe, just, monad, bind,
                            applicative)
  where

import Control.Applicative
import Control.Arrow (first, second, left, right)
import Control.Monad (liftM)
import Data.Char (toUpper)
import Data.Maybe (isJust, fromJust, maybe)
import Data.IORef

-- |Semantic Editor Combinator on the result of an unary function
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result =  (.)

-- |Semantic Editor Combinator on each value of a list
each :: (a -> b) -> ([a] -> [b])
each = fmap

-- |Using 'set' one can set instead of modify a value using Semantic Editor Combinators
--  for example '(first.set) 1' will set the first value of a tuple to 1
set :: a -> b -> a
set = const

-- |Semantic Editor Combinator for Maybe
just ::  (a -> b) -> Maybe a -> Maybe b
just = monad

-- |Semantic Editor Combinator for monads
monad :: Monad m => (a -> b) -> m a -> m b
monad = liftM -- (>>= return . f)

-- |Semantic Editor Combinator for monadicaly transforming a monadic value
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = (>>= f)

-- |Semantic Editor Combinator for applicatives
applicative :: Applicative f => (a -> b) -> f a -> f b
applicative = fmap

-- |Semantic Editor Combinator on argument of an unary function
argument :: (a' -> a) -> ((a -> b) -> (a' -> b))
argument = flip (.)

ioref ::  (a -> a) -> IORef a -> IO ()
ioref = flip modifyIORef 

-- |Semantic Editor Combinator applying the given function only when the given predicate
--  yields true for an input value.
editIf :: (a -> Bool) -> (a -> a) -> (a -> a)
editIf p f a = if p a then f a else a
