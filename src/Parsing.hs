{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Parsing
-}

module Parsing (
    parseInput,
    List(..)
) where

import Lib
import Error
import ErrorHandling

data List = List {
    listOfTruples :: [(Int, Int, Int)],
    listOfTuples :: [(Int, Int)]
} deriving Show

defaultList :: List
defaultList = List {
    listOfTruples = [],
    listOfTuples = []
}

processStrings :: [String] -> List -> List
processStrings [] list = list
processStrings (x:xs) (List t3s t2s)
    | countNumbers x == 1 = processStrings xs
        (List t3s ((read x :: (Int, Int)) : t2s))
    | countNumbers x == 2 = processStrings xs
        (List ((read x :: (Int, Int, Int)) : t3s) t2s)
    | otherwise = processStrings xs (List t3s t2s)

splitStr :: String -> [String]
splitStr [] = []
splitStr (x:xs)
    | x `elem` " " = splitStr xs
    | otherwise = tuple : splitStr rest
    where
        (tuple, rest) = break (`elem` " ") (x:xs)

parseInput :: [String] -> IO List
parseInput args = do
    contentFile <- readFile (args !! 5)
    let contentFileTolines = lines contentFile
        test = unwords contentFileTolines
        content = splitStr test
        result = processStrings content defaultList
    checkLengthFromList (countElements content)
    return result
