module Data.HRTree.HilbertTest where

import Data.Word
import Data.HRTree.Geometry
import Data.HRTree.GeometryTest () -- pull in arbitrary instances
import Data.HRTree.Hilbert

import Test.QuickCheck

newtype PositiveInt16 = PosInt16 {
    v :: Int
    }
instance Arbitrary PositiveInt16 where
    -- Limited to 0-32767 to avoid the hilbert value being greater than the maximum int and causing overflow.
    arbitrary = fmap (PosInt16 . flip mod 32768) arbitrary

instance Show PositiveInt16 where
    show (PosInt16 v) = show v


prop_AlwaysNonNegative :: PositiveInt16 -> PositiveInt16 -> Bool
prop_AlwaysNonNegative (PosInt16 x) (PosInt16 y) = hilbertDistance 16 (x, y) >= 0


-- |Run all the tests.
testAll :: IO ()
testAll = do
    quickCheck prop_AlwaysNonNegative
