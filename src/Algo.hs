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
