module ReadRects ( readRectsFromFile
                 , readRectFromSTDIN
                 , Rectangle
                 ) where

import Prelude hiding (takeWhile)
import Data.HRTree.Geometry
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Attoparsec as P
import Control.Applicative ((<$>),(<*>), (<*), (*>))
import Data.Char (ord)
import System.IO (stdin)
import Text.Printf

data Rectangle = Rect Point Point Point Point deriving (Eq)

instance Show Rectangle where
    show (Rect (Point p1x p1y)
               (Point p2x p2y)
               (Point p3x p3y)
               (Point p4x p4y)) = printf "%d,%d,%d,%d,%d,%d,%d,%d" p1x p1y p2x p2y p3x p3y p4x p4y

instance SpatiallyBounded Rectangle where
    boundingBox (Rect p1 p2 p3 p4) = boundingBox [p1,p2,p3,p4]

parseRect :: Parser Rectangle
parseRect = Rect <$> point
                 <* separator <*> point
                 <* separator <*> point
                 <* separator <*> point
    where
        point       = Point <$> number <* separator <*> number
        number      = (read . C8.unpack) <$> takeWhile isDigit
        separator   = satisfy isSeparator
        isDigit w   = w >= 48 && w <= 57 -- any digit
        isSeparator = (== 44) -- Comma

readRectsFromFile :: FilePath -> IO (Either String [Rectangle])
readRectsFromFile path =
    parseOnly (many (parseRect <* eol)) <$> BS.readFile path
    where
        eol         = satisfy isEol
        isEol       = (== 10)

readRectFromSTDIN :: IO (Either String Rectangle)
readRectFromSTDIN = parseOnly parseRect <$> BS.hGetLine stdin
