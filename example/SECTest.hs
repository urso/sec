{-# LANGUAGE TemplateHaskell #-}

-- load me with ghci and play

module SECTest where

import Data.SemanticEditors

type Name = String
type Street = String
type City = String
type ZipCode = Int

data Person = Person
    { age     :: Int
    , name    :: Name
    , address :: Address
    }
  deriving (Show)

data Address = Address
    { street  :: Street
    , city    :: City
    , zipCode :: ZipCode
    }
  deriving(Show)

mkEditors [''Person, ''Address]

setAge ::  Int -> Person -> Person
setAge     = editAge.set

setName ::  Name -> Person -> Person
setName    = editName.set

setZipCode ::  ZipCode -> Person -> Person
setZipCode = editAddress.editZipCode.set

setStreet ::  Street -> Person -> Person
setStreet  = editAddress.editStreet.set

setCity ::  City -> Person -> Person
setCity    = editAddress.editCity.set

joe ::  Person
joe = Person 24 "joe" $ Address "any street" "LA" 12345

mary ::  Person
mary = Person 22 "mary" $ Address "another street" "NYC" 54321

couple ::  [Person]
couple = [joe, mary]

moveTogether ::  Address -> [Person] -> [Person]
moveTogether = each.editAddress.set

