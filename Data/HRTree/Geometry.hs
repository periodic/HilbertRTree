{-# LANGUAGE TemplateHaskell #-}
module Data.HRTree.Geometry ( Point(..)
                            , BoundingBox (..)
                            , SpatiallyBounded (..)
                            , bbIntersect, bbCovers
                            ) where

import Data.Monoid
import Data.Word

import Data.Binary
import Data.DeriveTH

-- | A point just references a point in two-dimensional space, coordinates are given as ints.
data Point = Point Word16 Word16 deriving (Eq, Ord)

instance Show Point where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Bounded Point where
  minBound = Point minBound minBound
  maxBound = Point maxBound maxBound

-- | A bounding box is a rectangle, and so only needs to specify the bottom-left point and top-right, given by the min/max
data BoundingBox = BoundingBox { bbMin :: Point
                               , bbMax :: Point
                               }
                 | EmptyBox
                 deriving (Eq, Ord)

$(derives [makeBinary] [''Point, ''BoundingBox])


instance Show BoundingBox where
    show EmptyBox = "<Empty Box>"
    show (BoundingBox min max) = "<" ++ show min ++ "," ++ show max ++ ">"

-- | Bounding boxes are a monoid.
instance Monoid BoundingBox where
    mempty = EmptyBox

    mappend EmptyBox a = a
    mappend a EmptyBox = a
    mappend a b = let (BoundingBox (Point minXA minYA) (Point maxXA maxYA)) = a
                      (BoundingBox (Point minXB minYB) (Point maxXB maxYB)) = b
                   in BoundingBox { bbMin = Point (min minXA minXB) (min minYA minYB)
                                  , bbMax = Point (max maxXA maxXB) (max maxYA maxYB)
                                  }

-- | A type class for things that can be bounded.
class SpatiallyBounded a where
    boundingBox :: a -> BoundingBox
    center      :: a -> Maybe Point
    center = center . boundingBox

instance SpatiallyBounded BoundingBox where
    boundingBox box = box
    center EmptyBox = Nothing
    center (BoundingBox (Point x1 y1) (Point x2 y2)) = let sx = fi x1 + fi x2 :: Word32
                                                           sy = fi y1 + fi y2 :: Word32
                                                       in Just $ Point (fi $ sx `div` 2) (fi $ sy `div` 2)

instance SpatiallyBounded Point where
    boundingBox p = BoundingBox p p

instance (SpatiallyBounded a, SpatiallyBounded b) => SpatiallyBounded (a, b) where
    boundingBox (a, b) = boundingBox a `mappend` boundingBox  b

instance SpatiallyBounded a => SpatiallyBounded [a] where
    boundingBox = foldr (mappend . boundingBox) EmptyBox

-- | Intersection of two boxes
bbIntersect :: (SpatiallyBounded a, SpatiallyBounded b) => a -> b -> Bool
bbIntersect a b | (boundingBox a == EmptyBox) || (boundingBox b == EmptyBox) = False 
                | otherwise
                = let BoundingBox (Point xa1 ya1) (Point xa2 ya2) = boundingBox a
                      BoundingBox (Point xb1 yb1) (Point xb2 yb2) = boundingBox b
                      intersect a1 a2 b1 b2 = if a1 <= b1
                                            then b1 <= a2
                                            else a1 <= b2
                  in intersect xa1 xa2 xb1 xb2 && intersect ya1 ya2 yb1 yb2

bbCovers a b = a `mappend` b == a

fi :: (Integral a, Num b) => a -> b
fi a = fromIntegral a
