module Data.HRTree.Internal where

import Data.HRTree.Geometry
import Data.HRTree.Hilbert
import Data.Word
import qualified Data.List as L (insert, unfoldr)

-- | Capacity constants.  TODO: Make these tunable.
nodeCapacity = 4
leafCapacity = 4
splitOrder = 1

-- | Type alias for the type we're going to use as a key.
type Key = Word32

-- | Individual records of an internal node.
data (SpatiallyBounded a) => NodeRecord a =
    NR { nrMbr   :: BoundingBox
       , nrKey   :: Key
       , nrChild :: RTree a
       }

-- Instances for the node-records.
instance (SpatiallyBounded a) => Eq (NodeRecord a) where
    n1 == n2 = nrKey n1 == nrKey n2
instance (SpatiallyBounded a) => Ord (NodeRecord a) where
    n1 <= n2 = nrKey n1 <= nrKey n2
instance (SpatiallyBounded a, Show a) => Show (NodeRecord a) where
    show (NR _ _ child) = show child

-- | Individual records of a leaf node.
data (SpatiallyBounded a) => LeafRecord a =
    LR { lrMbr   :: BoundingBox
       , lrKey   :: Key
       , lrObj   :: a
       }

-- Instances for leaf-records
instance (SpatiallyBounded a) => Eq (LeafRecord a) where
    l1 == l2 = lrKey l1 == lrKey l2
instance (SpatiallyBounded a) => Ord (LeafRecord a) where
    l1 <= l2 = lrKey l1 <= lrKey l2
instance (SpatiallyBounded a, Show a) => Show (LeafRecord a) where
    show (LR _ _ obj) = show obj

{- | The RTree type, which is really just a node, either an internal node or a leaf.
 -}
data (SpatiallyBounded a) => RTree a = Node [NodeRecord a]
                                     | Leaf [LeafRecord a]
-- | INstances for RTree
instance (SpatiallyBounded a) => SpatiallyBounded (RTree a) where
    boundingBox (Node records) = boundingBox . map nrMbr $ records
    boundingBox (Leaf records) = boundingBox . map lrMbr $ records
instance (SpatiallyBounded a, Show a) => Show (RTree a) where
    show (Node records) = "<" ++ show records ++ ">"
    show (Leaf records) = "<" ++ show records ++ ">"

{- | Create an empty RTree.  An empty tree is just a single leaf.
 -}
empty :: (SpatiallyBounded a) => RTree a
empty = Leaf []

{-| Insert a record into the tree.
 -}
insert :: (SpatiallyBounded a) => a -> RTree a -> RTree a
insert item tree =
    let record = makeLeafRecord item
     in case insertK record [tree] of
        [] -> error "Empty tree returned."
        [r] -> r
        rs  -> Node . map makeNodeRecord $ rs

{-| Search the tree, returning all records that have intersecting bounding boxes with the search item.
 -}
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

{------------------------------------------------------------
 - Non-exported functions
 ------------------------------------------------------------}

-- | Wrap a node in a record, getting the right key and mbr.
makeNodeRecord :: (SpatiallyBounded a) => RTree a -> NodeRecord a
makeNodeRecord t = NR (boundingBox t) (getNodeKey t) t

-- | Wrap an item in a record, calculating the key and mbr.
makeLeafRecord :: (SpatiallyBounded a) => a -> LeafRecord a
makeLeafRecord i = LR (boundingBox i) (getKey i) i

-- | Make a set of leaf records from a set of LeafRecords, respecting the max capacity.
makeLeaves :: (SpatiallyBounded a) => [LeafRecord a] -> [RTree a]
makeLeaves records = if length records <= leafCapacity
                     then [Leaf records]
                     else map Leaf . distribute (splitOrder + 1) $ records

-- | Make a set of node records from a set of NodeRecords, respecting the max capacity.
makeNodes :: (SpatiallyBounded a) => [NodeRecord a] -> [RTree a]
makeNodes records = if length records <= nodeCapacity
                     then [Node records]
                     else map Node . distribute (splitOrder + 1) $ records

-- | Split a list into n sublists evenly.
distribute :: Integer -> [a] -> [[a]]
distribute n xs = L.unfoldr split xs
    where
        limit = ceiling (fromIntegral (length xs) / fromIntegral n)
        split [] = Nothing
        split ys = Just . splitAt limit $ ys

-- | Get the key for this object.
getKey :: (SpatiallyBounded a) => a -> Key
getKey item = case center item of
                Just p -> hilbertValue 16 p
                Nothing -> 0

-- | Get the key for a node.
getNodeKey :: (SpatiallyBounded a) => RTree a -> Key
getNodeKey (Node records) = foldr (max . nrKey) 0 records
getNodeKey (Leaf records) = foldr (max . lrKey) 0 records

{- | Inserting a leaf record in the tree.
 -
 - For a leaf this just means taking the set of all leaf records and allocate
 - them to one or more nodes.
 -
 - For a node this requires we find the right sub-node to insert in, which is
 - the first node for which the key is greater than or equal to the key for the
 - object we are inserting.
 -}
insertK :: (SpatiallyBounded a) => LeafRecord a -> [RTree a] -> [RTree a]
insertK r leaves@((Leaf _):_) = makeLeaves . L.insert r . concatMap (\(Leaf records) -> records) $ leaves

insertK leaf@(LR _ key _) nodes =
    let findNode [] = return . makeNodeRecord . Leaf . return $ leaf
        findNode (r:[]) = map makeNodeRecord $ insertK leaf [nrChild r]
        findNode (r:rs) = if nrKey r >= key
                          then map makeNodeRecord (insertK leaf [nrChild r]) ++ rs
                          else r : findNode rs
     in makeNodes . findNode . concatMap (\(Node records) -> records) $ nodes

