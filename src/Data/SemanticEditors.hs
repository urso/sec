
module Data.SemanticEditors(result, first, second, each, set, argument, 
                            left, right, ioref, maybe,
                            mkEditors, mkEditor) 
  where

import Control.Applicative
import Control.Arrow (first, second)
import Data.Char (toUpper)
import Data.Maybe (isJust, fromJust, maybe)
import Language.Haskell.TH.Syntax
import Data.IORef

-- |Semantic editor on the result of an unary function
result :: (b -> b') -> ((a -> b) -> (a -> b'))
result =  (.)

-- |Semantic editor on each value of a list
each :: (a -> b) -> ([a] -> [b])
each = fmap

-- |Using 'set' one can set instead of modify a value using Semantic Editor Combinators
--  for example '(first.set) 1' will set the first value of a tuple to 1
set :: a -> b -> a
set = const

-- |Semantic editor for 'Left' values of type 'Either'
left :: (a -> a') -> Either a b -> Either a' b
left f = either (Left . f) (Right . id)

-- |Semantic editor for 'Right' values of type 'Either'
right ::  (b -> b') -> Either a b -> Either a b'
right f = either (Left . id) (Right . f)

-- |Semantic editor on argument of an unary function
argument :: (a' -> a) -> ((a -> b) -> (a' -> b))
argument = flip (.)

ioref ::  (a -> a) -> IORef a -> IO ()
ioref = flip modifyIORef 

infix 1 <.> -- chosen arbitrarily
f <.> g = (f <$>) . g

-- |mkEditors creates Semantic Editors for each data type given. More information see mkEditor
mkEditors :: [Name] -> Q [Dec]
mkEditors = concat <.> mapM mkEditor

-- |mkEditor creates Semantic editors for each named field in a given data type by
--  appending the fields name (first letter is converted to uppercase) to the name \"edit\".
--  If a fields name starts with an underscore \'_\' no editor will be generated
--
--  for example:
--
-- >  data Person = Person { age :: Integer, name :: String, _sex :: String }
--
--  will generate the editors editAge and editName:
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

