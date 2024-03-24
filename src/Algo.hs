--
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Algo
--

module Algo {
} where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Random
import Data.List (foldl')

data Clusters = Clusters {
    centroides :: (Int, Int, Int),
    pixels :: [(Int, Int, Int)]
} deriving Show

randomInt :: Int -> Int -> IO Int
randomInt min max = randomRIO (min, max)

createCluster :: (Int, Int, Int) -> Clusters
createCluster centroid = Clusters { centroides = centroid, pixels = [] }

generateClusters :: [(Int, Int, Int)] -> Int -> Int -> Int -> IO [Clusters]
generateClusters _ 0 _ _ = return []
generateClusters list n min max = do
    centroidValue <- randomInt min max
    centroid <- generateCentroides list centroidValue
    rest <- generateClusters list (n - 1) min max
    return (Clusters { centroides = centroid, pixels = [] } : rest)

generateCentroides :: [(Int, Int, Int)] -> Int -> IO (Int, Int, Int)
generateCentroides (x:_) 0 = return x
generateCentroides (_:xs) n = generateCentroides xs (n - 1)
generateCentroides [] _ = error "error"