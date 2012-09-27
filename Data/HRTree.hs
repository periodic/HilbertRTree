module Data.HRTree ( empty
                   , insert
                   , search
                   , searchNearest
                   , RTree
                   , BoundingBox (..)
                   , Point (..)
                   , SpatiallyBounded(..)
                   ) where

import Data.HRTree.Geometry
import Data.HRTree.Internal
import Data.HRTree.Utilities