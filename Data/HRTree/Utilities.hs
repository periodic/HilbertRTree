module Data.HRTree.Utilities where

import Data.HRTree.Geometry
import Data.HRTree.Internal

import Data.Int(Int64)
import Data.Word(Word16)
import Data.Ord(comparing)
import Data.List(sortBy,foldl')
import Data.Maybe(fromJust)

-- rebuilds the tree mapping a given function to it's elements
-- can't be a Functor because of class constraints
mapRTree :: (SpatiallyBounded a, SpatiallyBounded b) => (a -> b) -> RTree a -> RTree b
mapRTree f tree = let elems = search (BoundingBox (Point minBound minBound) (Point maxBound maxBound)) tree
                  in foldl' (flip insert) empty $ map f elems

-- Returns elements of the tree in order of increasing distance from the given point
searchNearest :: SpatiallyBounded a => Point -> RTree a -> [a]
searchNearest p0@(Point x0 y0) t = sort_nearest p0 0 $ map (\(bb,sqr) -> (search bb t, sqr)) (squares p0)

sort_nearest :: SpatiallyBounded a => Point -> Int64 -> [([a],Int64)] -> [a]
sort_nearest _ _ [] = []
sort_nearest p0 prev_sqr ((cur,cur_sqr):rest) = sorted ++ sort_nearest p0 cur_sqr rest
  where
    new = filter (\p -> let d = sqr_d p in prev_sqr <= d && d < cur_sqr) cur
    sorted = sortBy (comparing sqr_d) new
    sqr_d pp = sqr_dist p0 (fromJust $ center pp)

sqr_dist :: Point -> Point -> Int64
sqr_dist (Point x1 y1) (Point x2 y2) = (fi x2  - fi x1)^2 + (fi y2 - fi y1)^2

-- растущие квадраты и квадраты их полустороны
squares :: Point -> [(BoundingBox,Int64)]
squares (Point x0 y0) = zip growing_boxes (map (^2) powers)
  where
    x0_i = fi x0
    y0_i = fi y0
    powers = iterate (*2) 2 :: [Int64]
    x1s = map (bound . (x0_i -)) powers
    y1s = map (bound . (y0_i -)) powers
    x2s = map (bound . (x0_i +)) powers
    y2s = map (bound . (y0_i +)) powers
    points1 = zipWith Point x1s y1s
    points2 = zipWith Point x2s y2s
    boxes = zipWith BoundingBox points1 points2
    growing_boxes = until_eq boxes
    bound x | x < fi (minBound :: Word16) = 0
            | x > fi (maxBound :: Word16) = maxBound
            | True = fi x

until_eq (x1:xs@(x2:_)) | x1 == x2 = [x1]
                        | True = x1: until_eq xs
until_eq _ = error "until_eq should be applied to infinite streams"

fi a = fromIntegral a
