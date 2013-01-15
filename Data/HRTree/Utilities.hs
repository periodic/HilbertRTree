module Data.HRTree.Utilities where

import Data.HRTree.Geometry
import Data.HRTree.Internal

import Data.Int(Int64)
import Data.Word(Word16)
import Data.Ord(comparing)
import Data.List(sortBy,foldl')
import Data.Maybe(fromJust, catMaybes)

-- rebuilds the tree mapping a given function to it's elements
-- can't be a Functor because of class constraints
mapRTree :: (SpatiallyBounded a, SpatiallyBounded b) => (a -> b) -> RTree a -> RTree b
mapRTree f tree = let elems = search (BoundingBox (Point minBound minBound) (Point maxBound maxBound)) tree
                  in foldl' (flip insert) empty $ map f elems

-- Returns elements of the tree in order of increasing distance from the center of given object
searchNearest :: (SpatiallyBounded a, SpatiallyBounded b) => (b -> a -> Float) -> b -> RTree a -> [a]
searchNearest metric p0 t = sort_nearest (metric p0) 0 $ map (\(bb,sqr) -> (search bb t, sqr)) (squares . fromJust . center $ p0)

sort_nearest :: (SpatiallyBounded a) => (a -> Float) -> Int64 -> [([a],Int64)] -> [a]
sort_nearest _ _ [] = []
sort_nearest metric1 prev_size ((cur,cur_size):rest) = sorted ++ sort_nearest metric1 cur_size rest
  where
    new = filter (\p -> let d = metric1 p in fi prev_size <= d && d < fi cur_size) cur
    sorted = sortBy (comparing metric1) new

-- a sequence of Growing squares and their sizes
squares :: Point -> [(BoundingBox,Int64)]
squares (Point x0 y0) = zip growing_boxes powers
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

-- | Modifies a region of a tree. If function returns Nothing deletes a value, else updates old value with a returned one.
-- | If Bounding box corresponing to value is changed, the result is undefined. Please use mapRTree in this case.
modifyRegion :: (SpatiallyBounded a) => (a -> Maybe a) -> BoundingBox  ->  RTree a -> RTree a
modifyRegion update region = clear_mbr
  where wrap_filled c mlst = c (catMaybes mlst)
        clear_mbr (Node records) = wrap_filled Node (map proc_node_record records)
        clear_mbr (Leaf records) = wrap_filled Leaf (map proc_leaf_record records)
        proc_node_record nr = if bbIntersect (nrMbr nr) region then Just . makeNodeRecord . clear_mbr $ nrChild nr else Just nr
        proc_leaf_record lr = if bbIntersect (lrMbr lr) region then update (lrObj lr) >>= return . makeLeafRecord else Just lr
