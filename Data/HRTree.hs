module Data.HRTree ( empty
                   , insert
                   , clear
                   , search
                   , searchNearest
                   , mapRTree
                   , RTree
                   , BoundingBox (..)
                   , Point (..)
                   , SpatiallyBounded(..)
                   ) where

import Data.HRTree.Geometry
import Data.HRTree.Internal
import Data.HRTree.Utilities