module Data.HRTree.GeometryTest where

import Test.QuickCheck
import Data.HRTree.Geometry

import Control.Monad

-- | Arbitrary instances.
instance Arbitrary Point where
    arbitrary = liftM2 Point arbitrary arbitrary

instance Arbitrary BoundingBox where
    arbitrary = oneof [ do minB@(Point x1 y1) <- arbitrary
                           maxB <- arbitrary `suchThat` (\(Point x2 y2) -> x1 <= x2 && y1 <= y2)
                           return $ BoundingBox minB maxB
                      , return EmptyBox
                      ]

-- | Various geometric features that are not EmptyBox.
data GeometricFeature = FeatPoint Point
                      | FeatLine  [Point]
                      deriving (Show, Eq, Ord)

instance SpatiallyBounded GeometricFeature where
    boundingBox (FeatPoint p) = boundingBox p
    boundingBox (FeatLine ps) = boundingBox ps

instance Arbitrary GeometricFeature where
    arbitrary = oneof [ fmap FeatPoint arbitrary
                      , fmap FeatLine (liftM2 (:) arbitrary arbitrary)
                      ]


-- |Bounding boxes should be associative.
prop_Associative :: GeometricFeature -> GeometricFeature -> GeometricFeature -> Bool
prop_Associative p1 p2 p3 = boundingBox (boundingBox (p1, p2), p3) == boundingBox(p1, boundingBox (p2, p3))

-- |Every geometric feature should intesect itself.
prop_BBIntersectSelf :: GeometricFeature -> Bool
prop_BBIntersectSelf box = bbIntersect box box

-- |For every feature, each point itself should have a bounding box that intersects the box that bounds them all.
prop_BoxIsBigger :: [GeometricFeature] -> Bool
prop_BoxIsBigger points = let box = boundingBox points
                          in all (bbIntersect box) points

-- |Run all the tests.
testAll :: IO ()
testAll = do
    putStrLn "boundingBox is associative."
    quickCheck prop_Associative
    putStrLn "Bounding boxes intersect themselves."
    quickCheck prop_BBIntersectSelf
    putStrLn "A bounding box always contains every point used to create it."
    quickCheck prop_BoxIsBigger
