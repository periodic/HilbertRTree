module Main where

import Data.HRTree.Internal
import Data.HRTree.Geometry

import Data.HRTree.GeometryTest
import Data.HRTree.HilbertTest

import Test.QuickCheck

import Control.Applicative ((<$>))

instance (SpatiallyBounded a, Arbitrary a) => Arbitrary (RTree a) where
    arbitrary = foldr insert empty <$> arbitrary

instance (SpatiallyBounded a, Arbitrary a) => Arbitrary (NodeRecord a) where
    arbitrary = makeNodeRecord <$> arbitrary

instance (SpatiallyBounded a, Arbitrary a) => Arbitrary (LeafRecord a) where
    arbitrary = makeLeafRecord <$> arbitrary


prop_InsertedValuesAreFound :: GeometricFeature -> RTree GeometricFeature -> Bool
prop_InsertedValuesAreFound feat tree = elem feat . search feat . insert feat $ tree

prop_FindOneMore :: GeometricFeature -> RTree GeometricFeature -> Bool
prop_FindOneMore feat tree = (length . search feat $ tree) + 1 == (length . search feat . insert feat $ tree)

prop_NoOverflow :: RTree GeometricFeature -> Bool
prop_NoOverflow (Node records) = let thisNode = length records <= nodeCapacity
                                     childNodes = map (prop_NoOverflow . nrChild) records
                                  in and (thisNode : childNodes)
prop_NoOverflow (Leaf records) = length records <= leafCapacity


testAll = do
    Data.HRTree.GeometryTest.testAll
    Data.HRTree.HilbertTest.testAll
    putStrLn "Inserted values are found in a search."
    quickCheck prop_InsertedValuesAreFound
    putStrLn "After an insert, we should always find one more value on a search than before."
    quickCheck prop_FindOneMore
    putStrLn "No node should overflow."
    quickCheck prop_NoOverflow

main = Main.testAll