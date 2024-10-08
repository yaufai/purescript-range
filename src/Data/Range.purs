module Data.Range
  ( RecordRange
  , class Range
  , elem
  , equivalent
  , include
  , intersect
  , isLowerOpen
  , isUpperOpen
  , isValid
  , lower
  , mkRecordRange
  , overlap
  , upper
  )
  where

import Prelude

class (Ord a) <= Range range a | range -> a where
  lower :: range -> a
  isLowerOpen :: range -> Boolean
  upper :: range -> a
  isUpperOpen :: range -> Boolean

data RecordRange a = RecordRange { lower     :: a
                                 , lowerOpen :: Boolean
                                 , upper     :: a
                                 , upperOpen :: Boolean
                                 }

instance (Ord a) => Range (RecordRange a) a where
  lower (RecordRange range) = range.lower
  upper (RecordRange range) = range.upper
  isLowerOpen (RecordRange range) = range.lowerOpen
  isUpperOpen (RecordRange range) = range.upperOpen

mkRecordRange :: forall a. Ord a => a -> Boolean -> a -> Boolean -> RecordRange a
mkRecordRange lower lowerOpen upper upperOpen = RecordRange { lower     : lower
                                                            , lowerOpen : lowerOpen
                                                            , upper     : upper
                                                            , upperOpen : upperOpen
                                                            }

isValid :: forall a range. Range range a => range -> Boolean
isValid range = (lower range) <= (upper range)

elem :: forall a range. Ord a => Range range a =>  a -> range -> Boolean
elem x range = inBetween || atLower || atUpper
  where inBetween = ((lower range) < x) && (x < (upper range))
        atLower   = (x == lower range) && (not $ isLowerOpen range)
        atUpper   = (x == upper range) && (not $ isUpperOpen range)

overlap :: forall a range. Ord a => Range range a => range -> range -> Boolean
overlap a b = ((lower a) <= (upper b)) && ((lower b) <= (upper a))

include :: forall a range. Range range a => range -> range -> Boolean
include x y  =  (upper x >= upper y)
             && (isValid y)
             && (lower x <= lower y)

intersect :: forall a range. Range range a => range -> range -> RecordRange a
intersect a b = RecordRange { lower     : max (lower a) (lower b)
                            , lowerOpen : newLowerOpen
                            , upper     : min (upper a) (upper b)
                            , upperOpen : newUpperOpen
                            }
                where newLowerOpen = (isLowerOpen a) || (isLowerOpen b)
                      newUpperOpen = (isUpperOpen a) || (isUpperOpen b)

equivalent :: forall a range. Range range a => range -> range -> Boolean
equivalent a b =  (lower a == lower b)
               && (isLowerOpen a == isLowerOpen b)
               && (upper a == upper b)
               && (isUpperOpen a == isUpperOpen b)