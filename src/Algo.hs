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

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance (r1, g1, b1) (r2, g2, b2) =
    let r = fromIntegral (r1 - r2)^2
        g = fromIntegral (g1 - g2)^2
        b = fromIntegral (b1 - b2)^2
        distanceSquared = r + g + b
    in sqrt distanceSquared

attributePixels :: [Clusters] -> [(Int, Int, Int)] -> IO [Clusters]
attributePixels clusters [] = return clusters
attributePixels clusters (pixel:pixels) = do
    let assignPixel c1 c2 =
            if distance (centroides c1) pixel <= distance (centroides c2) pixel
                then c1
                else c2
    let loop = foldl assignPixel (head clusters) (tail clusters)
    let updatedClusters = updateClusters clusters loop pixel
    attributePixels updatedClusters pixels

updateClusters :: [Clusters] -> Clusters -> (Int, Int, Int) -> [Clusters]
updateClusters [] _ _ = []
updateClusters (Clusters centroid' pixels':cs) (Clusters centroid _) pixel
    | centroid == centroid' = (addPixelToCluster (Clusters centroid' pixels') pixel) : updateClusters cs (Clusters centroid' pixels') pixel
    | otherwise = (Clusters centroid' pixels') : updateClusters cs (Clusters centroid pixels') pixel

addPixelToCluster :: Clusters -> (Int, Int, Int) -> Clusters
addPixelToCluster (Clusters centroid pixels) pixel = Clusters centroid (pixel:pixels)

calculateMean :: [(Int, Int, Int)] -> (Int, Int, Int)
calculateMean [] = (0, 0, 0)
calculateMean xs = let (sumR, sumG, sumB) = foldl' addTwoTuples (0, 0, 0) xs
                       n = length xs
                   in (sumR `div` n, sumG `div` n, sumB `div` n)

addTwoTuples :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTwoTuples (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

calculate :: Clusters -> IO Clusters
calculate (Clusters centroid pixels) = do
    let newCentroid = calculateMean pixels
    return (Clusters { centroides = newCentroid, pixels = [] })

calculateAll :: [Clusters] -> IO [Clusters]
calculateAll clusters = mapM calculate clusters

loop :: [Clusters] -> [(Int, Int, Int)] -> Float -> IO [Clusters]
loop clusters pixels convergenceLimit = loopUntilConvergence clusters
  where
    loopUntilConvergence prevClusters = do
        updatedClusters <- calculateAll prevClusters
        attributedClusters <- attributePixels updatedClusters pixels
        let convergenceReached = checkConvergence prevClusters attributedClusters convergenceLimit
        if convergenceReached
            then return attributedClusters
            else loopUntilConvergence attributedClusters

centroidsLimit :: Float -> (Clusters, Clusters) -> Bool
centroidsLimit limit (prevCluster, newCluster) =
    centroidDistance (centroides prevCluster) (centroides newCluster) <= limit

centroidDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
centroidDistance centroid1 centroid2 = distance centroid1 centroid2

checkConvergence :: [Clusters] -> [Clusters] -> Float -> Bool
checkConvergence prevClusters newClusters limit =
    all (centroidsLimit limit) (zip prevClusters newClusters)

searchPosition :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Int, Int)] -> Maybe (Int, Int)
searchPosition _ [] _ = Nothing
searchPosition pixel (p:ps) (pos:poss)
    | pixel == p = Just pos
    | otherwise = searchPosition pixel ps poss

printcluster :: [Clusters] -> [(Int, Int, Int)] -> [(Int, Int)] -> IO ()
printcluster [] _ _ = return ()
printcluster (cluster:clusters) list positions = do
    putStrLn "--"
    putStrLn (show (centroides cluster))
    putStrLn "-"
    printpixels (pixels cluster) list positions
    printcluster clusters list positions

printpixels :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> IO ()
printpixels [] _ _ = return ()
printpixels (pixel:pixels) list positions = do
    case searchPosition pixel list positions of
        Just pos -> putStrLn (show pos ++ " " ++ show pixel)
        Nothing -> putStrLn "Pixel not found in the list of positions"
    printpixels pixels list positions

algo :: [(Int, Int, Int)] -> [(Int, Int)] -> Int -> Int -> Int -> Float -> IO ()
algo pixels pos numberofcluster min max convergenceLimit = do
    clusters <- generateClusters pixels numberofcluster min max
    pixelsattribuer <- attributePixels clusters pixels
    finalClusters <- loop pixelsattribuer pixels convergenceLimit
    printcluster finalClusters pixels pos
