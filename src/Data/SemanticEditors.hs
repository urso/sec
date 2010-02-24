
module Data.SemanticEditors(result, first, second, each, editIf, set, argument, 
                            left, right, ioref, maybe, just, monad, bind,
                            applicative,
                            mkEditors, mkEditor, mkConstrTests) 
  where

import Control.Applicative
import Control.Arrow (first, second, left, right)
import Control.Monad (liftM)
import Data.Char (toUpper)
import Data.Maybe (isJust, fromJust, maybe)
import Language.Haskell.TH.Syntax
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

infix 1 <.> -- chosen arbitrarily
f <.> g = (f <$>) . g

-- |mkEditors creates Semantic Editor Combinators for each data type given. 
--  More information see mkEditor
mkEditors :: [Name] -> Q [Dec]
mkEditors = concat <.> mapM mkEditor

-- |mkEditor creates Semantic Editor Combinators for each named field in a given data type by
--  appending the fields name (first letter is converted to uppercase) to the name \"edit\".
--  If a fields name starts with an underscore \'_\' no editor will be generated
--
--  for example:
--
-- >  data Person = Person { age :: Integer, name :: String, _sex :: String }
--
--  will generate the lifters  editAge and editName:
--
-- @
--    editAge  f p = p { age = f (age p) }
--    editName f p = p { name = f (name p) }
-- @
--
mkEditor :: Name -> Q [Dec]
mkEditor name = do
    i <- reify name
    map (fromJust) . filter (isJust) <.> mapM mkEditor' . concatMap vars $
      case i of
        TyConI (DataD _ _ _ cs _) -> cs
        TyConI (NewtypeD _ _ _ c _) -> [c]
        _ -> []

  where vars (RecC _ v) = v

mkEditor' (name, _, _) = case nameBase name of
                           ('_':_)  -> return Nothing
                           (c:rest) -> Just <$> mkEditor'' ("edit" ++ (toUpper c:rest))
  where 
    mkEditor'' :: String -> Q Dec
    mkEditor'' name' = return $
      FunD (mkName name')
           [Clause [VarP (mkName "f"), VarP (mkName "r")] (NormalB $ 
                   RecUpdE (VarE (mkName "r")) 
                           [(name, 
                             AppE (VarE (mkName "f")) 
                                  (AppE (VarE name) (VarE $ mkName "r")))
                           ]) []]

-- |Template Haskell function for automatically creating predicates testing the constructors of a 
--  given data type.
--  for example:
--
-- @
--   data Color = Red | Green | Blue
--  $(mkConstrTests [''Color])
-- @
  --
--  will generate the following functions:
--
-- @
--   isRed Red     = True
--   isRed _       = False
--   isGreen Green = True
--   isGreen _     = False
--   isBlue Blue   = True
--   isBlue _      = False
-- @
--
mkConstrTests :: [Name] -> Q [Dec]
mkConstrTests = concat <.> mapM mk 
  where
    mk name = do
      i <- reify name
      map fromJust . filter isJust <.> mapM mkPredicate $
        case i of
          TyConI (DataD _ _ _ cs _) -> cs
          _ -> []

    mkPredicate (NormalC name ts) = Just <$> mkPredicate' name (length ts)
    mkPredicate (RecC name ts)   = Just <$> mkPredicate' name (length ts)
    mkPredicate _                = return Nothing

    mkPredicate' name argc = return $
      FunD (predName name)
        [ Clause [ConP name $ replicate argc WildP] (NormalB $ ConE (mkName "True")) []
        , Clause [WildP] (NormalB $ ConE (mkName "False")) []
        ]

    predName name = mkName ("is" ++ nameBase name)

