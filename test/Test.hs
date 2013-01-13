module Main where

import Data.HRTree.Internal
import Data.HRTree.Geometry
import Data.HRTree.Utilities

import Data.HRTree.GeometryTest
import Data.HRTree.HilbertTest
import Data.HRTree.UtilitiesTest

import qualified Data.List as L

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

prop_modifyRegionClears :: RTree GeometricFeature -> BoundingBox -> Bool
prop_modifyRegionClears t1 b =
  let u = BoundingBox minBound maxBound
      ps1 = search u t1
      t2 = modifyRegion (const Nothing) b t1
      outer = search u t2
      inner = filter (bbIntersect b) ps1
  in  L.sort ps1 == L.sort (outer ++ inner)

prop_modifyRegionMaps :: RTree Point3D -> Bool
prop_modifyRegionMaps t1 =
  let f (Point3D x' y' z') = Point3D x' y' (x' + y')
      u = BoundingBox minBound maxBound
      ps1 = search u t1
      t2 = modifyRegion (Just . f) u t1
      ps2 = search u t2
  in map f ps1 == ps2

testAll = do
    Data.HRTree.GeometryTest.testAll
    Data.HRTree.HilbertTest.testAll
    Data.HRTree.UtilitiesTest.testAll
    putStrLn "Inserted values are found in a search."
    quickCheck prop_InsertedValuesAreFound
    putStrLn "Region clearing works as expected"
    quickCheck prop_modifyRegionClears
    putStrLn "Point modifying works"
    quickCheck prop_modifyRegionMaps
    putStrLn "Functor axioms for mapRTree"
    quickCheck prop_functor_axiom
    putStrLn "After an insert, we should always find one more value on a search than before."
    quickCheck prop_FindOneMore
    putStrLn "No node should overflow."
    quickCheck prop_NoOverflow

main = Main.testAll
