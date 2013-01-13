module Data.HRTree.UtilitiesTest where

import Test.QuickCheck
import Control.Monad

import Data.HRTree.GeometryTest

import Data.HRTree
import Data.HRTree.Utilities

import Data.List(sort, sortBy)
import Data.Ord(comparing)

instance Show (a->b) where
  show f = "<function>"

data Point3D = Point3D {x:: Float, y:: Float, z:: Float} deriving (Eq, Show)

instance Arbitrary Point3D where
  arbitrary = do [x,y,z] <- replicateM 3 ( arbitrary `suchThat` \x -> x > 0 && x < 65000)
                 return $ Point3D x y z

instance SpatiallyBounded Point3D where
  boundingBox (Point3D x y z) = boundingBox $ Point (round x) (round y)

instance Metric Point3D where
  (Point3D x1 y1 z1) `metric` (Point3D x2 y2 z2) = sqrt $ (x1 - x2) ^2 + (y1 - y2) ^2 + (z1 - z2) ^ 2

growing (x1:x2:xs) = x1 <= x2 && growing (x2:xs)
growing _ = True

test_search :: Point3D -> [Point3D] -> Bool
test_search center points =
  let tree = foldr insert empty points
      sorted_points = searchNearest center tree
      dists = map (metric center) sorted_points
      dists2 = map (metric center) $ sortBy (comparing (metric center)) points
  in growing dists && dists == dists2

prop_functor_axiom :: (GeometricFeature -> GeometricFeature) -> (GeometricFeature -> GeometricFeature) -> RTree GeometricFeature -> Bool
prop_functor_axiom f1 f2 tree = let u = BoundingBox minBound maxBound
                                    ps1 = search u  $ (mapRTree f1 . mapRTree f2) tree
                                    ps2 = search u $  mapRTree (f1 . f2) tree
                                in sort ps1 == sort ps2

testAll = do
  putStrLn "Testing searchNearest"
  quickCheck test_search
