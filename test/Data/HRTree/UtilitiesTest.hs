module Data.HRTree.UtilitiesTest where

import Test.QuickCheck
import Control.Monad

import Data.HRTree.GeometryTest

import Data.HRTree
import Data.HRTree.Utilities

import Data.List(sort)

instance Show (a->b) where
  show f = "<function>"

growing (x1:x2:xs) = x1 <= x2 && growing (x2:xs)
growing _ = True

test_search center points =
  let tree = foldr insert empty points
      sorted_points = searchNearest center tree
      dists = map (sqr_dist center) sorted_points
  in growing dists

prop_functor_axiom :: (GeometricFeature -> GeometricFeature) -> (GeometricFeature -> GeometricFeature) -> RTree GeometricFeature -> Bool
prop_functor_axiom f1 f2 tree = let u = BoundingBox minBound maxBound
                                    ps1 = search u  $ (mapRTree f1 . mapRTree f2) tree
                                    ps2 = search u $  mapRTree (f1 . f2) tree
                                in sort ps1 == sort ps2

testAll = do
  putStrLn "Testing searchNearest"
  quickCheck test_search
