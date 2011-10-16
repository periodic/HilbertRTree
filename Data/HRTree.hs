module Data.HRTree where

import Data.HRTree.Geometry
import Data.List as L (insert, intercalate)

type Key = Int

data (SpatiallyBounded a) => NodeRecord a =
    NR { nrMbr   :: BoundingBox
       , nrKey   :: Key
       , nrChild :: RTree a
       } deriving (Eq)

instance (SpatiallyBounded a, Eq a) => Ord (NodeRecord a) where
    n1 <= n2 = nrKey n1 <= nrKey n2
instance (SpatiallyBounded a, Show a) => Show (NodeRecord a) where
    show (NR _ _ child) = show child

data (SpatiallyBounded a) => LeafRecord a =
    LR { lrMbr   :: BoundingBox
       , lrKey   :: Key
       , lrObj   :: a
       } deriving (Eq)

instance (SpatiallyBounded a, Eq a) => Ord (LeafRecord a) where
    l1 <= l2 = lrKey l1 <= lrKey l2
instance (SpatiallyBounded a, Show a) => Show (LeafRecord a) where
    show (LR _ _ obj) = show obj

data (SpatiallyBounded a) => RTree a = Node [NodeRecord a]
                                     | Leaf [LeafRecord a]
                                     deriving (Eq)
instance (SpatiallyBounded a) => SpatiallyBounded (RTree a) where
    boundingBox (Node records) = boundingBox . map nrMbr $ records
    boundingBox (Leaf records) = boundingBox . map lrMbr $ records
instance (SpatiallyBounded a, Show a) => Show (RTree a) where
    show (Node records) = "<" ++ show records ++ ">"
    show (Leaf records) = "<" ++ show records ++ ">"

nodeCapacity = 4
leafCapacity = 4

empty :: (SpatiallyBounded a) => RTree a
empty = Leaf []

insert :: (SpatiallyBounded a, Eq a) => a -> RTree a -> RTree a
insert item tree =
    let record = makeLeafRecord item
     in case insert_k record tree of
        [] -> error "Empty tree returned."
        [r] -> r
        rs  -> Node . map makeNodeRecord $ rs

makeParent :: (SpatiallyBounded a) => [RTree a] -> RTree a
makeParent = undefined

makeNodeRecord :: (SpatiallyBounded a) => RTree a -> NodeRecord a
makeNodeRecord t = NR (boundingBox t) (getNodeKey t) t

makeLeafRecord :: (SpatiallyBounded a) => a -> LeafRecord a
makeLeafRecord i = LR (boundingBox i) (getKey i) i

makeLeaves :: (SpatiallyBounded a) => [LeafRecord a] -> [RTree a]
makeLeaves records = if (length records <= leafCapacity)
                     then [Leaf records]
                     else let (these, rest) = splitAt (leafCapacity `div` 2) records
                           in Leaf these : makeLeaves rest

makeNodes :: (SpatiallyBounded a) => [NodeRecord a] -> [RTree a]
makeNodes records = if (length records <= nodeCapacity)
                     then [Node records]
                     else let (these, rest) = splitAt (nodeCapacity `div` 2) records
                           in Node these : makeNodes rest

-- | Get the key for this object.  For now this will just be 1.
getKey :: (SpatiallyBounded a) => a -> Key
getKey item = case center item of
            Nothing -> 0
            Just (Point x y) -> x + y

getNodeKey :: (SpatiallyBounded a) => RTree a -> Key
getNodeKey (Node records) = foldr max 0 . map nrKey $ records
getNodeKey (Leaf records) = foldr max 0 . map lrKey $ records

-- | Internal insertion methods.
-- TODO: why is Eq needed here for insert?
insert_k :: (SpatiallyBounded a, Eq a) => LeafRecord a -> RTree a -> [RTree a]
insert_k r (Leaf records) = makeLeaves $ L.insert r records -- TODO: splitting

insert_k leaf@(LR _ key _) (Node records) =
    let findNode [] = return . makeNodeRecord . Leaf . return $ leaf
        findNode (r:[]) = map makeNodeRecord $ insert_k leaf (nrChild r)
        findNode (r:rs) = if nrKey r >= key
                          then (map makeNodeRecord $ insert_k leaf (nrChild r)) ++ rs
                          else r : findNode rs
     in makeNodes (findNode records) -- TODO: splitting

search :: (SpatiallyBounded a, SpatiallyBounded b) => a -> RTree b -> [b]
search target = let mbr = boundingBox target
                 in search_mbr mbr
    where
        checkNodeRecord mbr r = if bbIntersect mbr (nrMbr r)
                            then search_mbr mbr . nrChild $ r
                            else []
        checkLeafRecord mbr r = if bbIntersect mbr (lrMbr r)
                            then (:[]) . lrObj $ r
                            else []
        search_mbr mbr (Node records) = concatMap (checkNodeRecord mbr) records
        search_mbr mbr (Leaf records) = concatMap (checkLeafRecord mbr) records

