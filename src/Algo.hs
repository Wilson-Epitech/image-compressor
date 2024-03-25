{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Algo
-}

module Algo (
    algo
) where

import Data.List (delete)
import System.Random
import Data.List (foldl')
import Lib

data Clusters = Clusters {
    centroides :: (Int, Int, Int),
    pixels :: [(Int, Int, Int)]
} deriving Show

randomInt :: Int -> Int -> IO Int
randomInt minValue maxValue = randomRIO (minValue, maxValue)

generateClusters :: [(Int, Int, Int)] -> Int -> Int -> Int -> IO [Clusters]
generateClusters _ 0 _ _ = return []
generateClusters list n minValue maxValue = do
    centroidValue <- randomInt minValue maxValue
    centroid <- generateCentroides list centroidValue
    rest <- generateClusters list (n - 1) minValue maxValue
    return (Clusters { centroides = centroid, pixels = [] } : rest)

generateCentroides :: [(Int, Int, Int)] -> Int -> IO (Int, Int, Int)
generateCentroides (x:_) 0 = return x
generateCentroides (_:xs) n = generateCentroides xs (n - 1)
generateCentroides [] _ = error "error"

attributePixels :: [Clusters] -> [(Int, Int, Int)] -> IO [Clusters]
attributePixels clusters [] = return clusters
attributePixels clusters (pixel:_pixels) =
    let assignPixel c1 c2 =
            if distance (centroides c1) pixel <= distance (centroides c2) pixel
                then c1
                else c2
        iteration = foldl assignPixel (head clusters) (tail clusters)
        updatedClusters = updateClusters clusters iteration pixel
    in attributePixels updatedClusters _pixels

updateClusters :: [Clusters] -> Clusters -> (Int, Int, Int) -> [Clusters]
updateClusters [] _ _ = []
updateClusters (Clusters centroid' pixels':cs) (Clusters centroid _) pixel
    | centroid == centroid' =
        (addPixelToCluster (Clusters centroid' pixels') pixel) :
        updateClusters cs (Clusters centroid' pixels') pixel
    | otherwise = (Clusters centroid' pixels') :
        updateClusters cs (Clusters centroid pixels') pixel

addPixelToCluster :: Clusters -> (Int, Int, Int) -> Clusters
addPixelToCluster (Clusters centroid existingPixels) newPixel =
    Clusters centroid (newPixel:existingPixels)

calculateMean :: [(Int, Int, Int)] -> (Int, Int, Int)
calculateMean [] = (0, 0, 0)
calculateMean xs = let (sumR, sumG, sumB) = foldl' addTwoTuples (0, 0, 0) xs
                       n = length xs
                   in (sumR `div` n, sumG `div` n, sumB `div` n)

addTwoTuples :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTwoTuples (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

calculate :: Clusters -> IO Clusters
calculate (Clusters _ existingPixels) =
    let newCentroid = calculateMean existingPixels
    in return (Clusters { centroides = newCentroid, pixels = [] })

calculateAll :: [Clusters] -> IO [Clusters]
calculateAll clusters = mapM calculate clusters

loop :: [Clusters] -> [(Int, Int, Int)] -> Float -> IO [Clusters]
loop clusters inputPixels cvgLimit = loopUntilConvergence clusters
  where
    loopUntilConvergence prevClusters = do
        updatedClusters <- calculateAll prevClusters
        attrClusters <- attributePixels updatedClusters inputPixels
        let cvgReach = checkConvergence prevClusters attrClusters cvgLimit
        if cvgReach
            then return attrClusters
            else loopUntilConvergence attrClusters


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
searchPosition _ _ [] = Nothing
searchPosition pixel (p:ps) (pos:poss)
    | pixel == p = Just pos
    | otherwise = searchPosition pixel ps poss

printcluster :: [Clusters] -> [(Int, Int, Int)] -> [(Int, Int)] -> IO ()
printcluster [] _ _ = return ()
printcluster (cluster:clusters) list positions = 
    putStrLn "--" >>
    putStrLn (show (centroides cluster)) >>
    putStrLn "-" >>
    printpixels (pixels cluster) list positions >>
    printcluster clusters list positions

printpixels :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)] -> IO ()
printpixels [] _ _ = return ()
printpixels (p:ps) list positions =
    case searchPosition p list positions of
        Just pos ->
            putStrLn (show pos ++ " " ++ show p) >>
            let updatedList = delete p list
                updatedPositions = delete pos positions
            in printpixels ps updatedList updatedPositions
        Nothing ->
            putStrLn "Pixel not found in the list of positions" >>
            printpixels ps list positions

algo :: [(Int, Int, Int)] -> [(Int, Int)] -> Int -> Int -> Int -> Float -> IO ()
algo pxls pos numberofcluster minValue maxValue convergenceLimit = do
    clusters <- generateClusters pxls numberofcluster minValue maxValue
    pixelsattribuer <- attributePixels clusters pxls
    finalClusters <- loop pixelsattribuer pxls convergenceLimit
    printcluster finalClusters pxls pos
