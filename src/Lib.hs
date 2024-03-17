{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Lib
-}

module Lib (
    convertToString,
    distance
) where

import Data.List (intercalate)

convertToString :: [String] -> String
convertToString = intercalate ","

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance (r1, g1, b1) (r2, g2, b2) =
    let r = fromIntegral (r1 - r2)^(2 :: Int)
        g = fromIntegral (g1 - g2)^(2 :: Int)
        b = fromIntegral (b1 - b2)^(2 :: Int)
        distanceSquared = r + g + b
    in sqrt distanceSquared