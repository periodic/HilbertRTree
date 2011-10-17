module Main where

import ReadRects
import Data.HRTree
import System.Environment (getArgs)
import System.CPUTime
import Text.Printf

maxMatches = 4

main :: IO ()
main = do
    args <- getArgs
    case args of
        []       -> putStrLn "You must supply a file name!"
        (path:_) -> preloadLoop path

preloadLoop :: FilePath -> IO ()
preloadLoop path = do
    putStr $ path ++ ": "
    ((count, tree), dt) <- time 10e9 loadRects
    printf "%d rectangles read in %f milliseconds\n" count dt
    interactiveLoop tree
    where
        loadRects = do
            eRects <- readRectsFromFile path
            case eRects of
                Left msg -> putStr "Error: " >> putStrLn msg >> return (0, empty)
                Right rects -> let tree = foldr insert empty rects
                                   count= length rects
                                in count `seq` tree `seq` return (count, tree)
    
interactiveLoop :: RTree Rectangle -> IO ()
interactiveLoop tree = loop
    where
        loop = do
            putStr "> "
            eRect <- readRectFromSTDIN
            case eRect of
                Left msg -> putStrLn "Unable to read input." >> loop
                Right rect -> timeSearch rect tree >> loop
        timeSearch r t = do
            let matches = search r t
            (m, dt) <- time 10e6 (return (length matches `seq` matches))
            printf "found %d matches in %f microseconds.\n" (length m) dt
            mapM_ print . take maxMatches $ m


time :: Double -> IO a -> IO (a, Double)
time scale a = do
        t1 <- getCPUTime
        r <- a
        t2 <- r `seq` getCPUTime
        let dt = fromIntegral (t2 - t1) / scale
        return (r, dt)
