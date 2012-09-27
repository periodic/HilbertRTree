module Data.HRTree.UtilitiesTest where

import Test.QuickCheck
import Control.Monad

import Data.HRTree
import Data.HRTree.Utilities


instance Arbitrary Point where
    arbitrary = liftM2 Point arbitrary arbitrary

growing (x1:x2:xs) = x1 <= x2 && growing (x2:xs)
growing _ = True

test_search center points =
  let tree = foldr insert empty points
      sorted_points = searchNearest center tree
      dists = map (sqr_dist center) sorted_points
  in growing dists

testAll = do
  putStrLn "Testing searchNearest"
  quickCheck test_search