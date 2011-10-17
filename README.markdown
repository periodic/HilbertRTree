# Hilbert R Tree

This project is a simple implementation of a (Hilbert R Tree)[http://en.wikipedia.org/wiki/Hilbert_R-tree].  It uses the Hilbert Curve to order the keys in an RTree, which places close points relatively close in the tree.  This makes it efficient to find other near points or intersecting points because much of the time all the geometric features inside a bounding box are all on the same branch of the tree.

## Implementation

This tree can store any value that is an instance of the SpatiallyBounded type-class.  This type-class provides two functions,

    boundingBox :: a -> BoundingBox
    center :: a -> Point

The first function returns a box (rectangle) that bounds all points of the object.  The second provides a center point which is used to approximate the Hilbert value.  A minimum complete definition is just `boundingBox`.  If center is not provided it will return the center of the boundingBox.  The definition, along with Point and BoundingBox, can be found in Data.HRTree.Geometry.

The tree itself provides four three methods

    empty :: SpatiallyBounded a => RTree a
    insert :: SpatiallyBounded a => a -> RTree a -> RTree a
    search :: SpatiallyBounded a => a -> RTree a -> [a]

They should be self-explanatory, but note that search returns all objects in the tree whose bounding box intersects the object provided.

The impementation of the hilbert value is from Bryan O'Sullivan's (work)[http://www.serpentine.com/blog/2007/01/11/two-dimensional-spatial-hashing-with-space-filling-curves/].

## Future Enhancements

Possible future enhancements include:

* Separating the RTree and Hilbert definitions so that any function can be provided, such as the Lebesgue distance.
* Tunable capacity.
* 3-2 splitting.
* More tests!
