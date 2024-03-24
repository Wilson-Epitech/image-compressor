{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- ErrorHandling
-}
    
module ErrorHandling (
    errorHandling,
    checkLengthFromList
) where

import Data.Maybe (isNothing)
import GetArgs
import Lib
import Error
import Usage

-- deplacer isEven et countElements dans lib.hs

-- data List = List {
--     listOfTruples :: [(Int, Int, Int)],
--     listOfTuples :: [(Int, Int)]
-- } deriving Show

-- defaultList :: List
-- defaultList = List {
--     listOfTruples = [],
--     listOfTuples = []
-- }

-- isEven :: Int -> Bool
-- isEven n
--     | n `mod` 2 == 0 = False
--     | otherwise = True

-- countElements :: [String] -> Int
-- countElements = length

-- checkLengthFromList :: Int -> IO ()
-- checkLengthFromList n
--     | isEven n == True = exitWith84error
--     | otherwise = return ()

-- processStrings :: [String] -> List -> List
-- processStrings [] list = list
-- processStrings (x:xs) (List t3s t2s)
--     | countNumbers x == 1 = processStrings xs
--         (List t3s ((read x :: (Int, Int)) : t2s))
--     | countNumbers x == 2 = processStrings xs
--         (List ((read x :: (Int, Int, Int)) : t3s) t2s)
--     | otherwise = processStrings xs (List t3s t2s)

-- splitStr :: String -> [String]
-- splitStr [] = []
-- splitStr (x:xs)
--     | x `elem` " " = splitStr xs
--     | otherwise = tuple : splitStr rest
--     where
--         (tuple, rest) = break (`elem` " ") (x:xs)

errorHandling :: [String] -> IO ()
errorHandling args = do
    -- args <- getArgs
    checkNbArgs args
    checkOrderArgs args
    let conf = getOpts defaultConf args
    checkNegativeArgs conf
    checkDataConf conf
    -- contentFile <- readFile (args !! 5)
    -- let contentFileTolines = lines contentFile
    --     test = unwords contentFileTolines
    --     content = splitStr test
    --     result = processStrings content defaultList
    -- -- checkLengthFromList (countElements content)
    -- print result

checkNegativeArgs :: Conf -> IO ()
checkNegativeArgs conf
    | (nbcolors conf) <= Just 0 = exitWith84error
    | (cvglimit conf) <= Just 0 = exitWith84error
    | otherwise = return ()

checkOrderArgs :: [String] -> IO ()
checkOrderArgs args
    | args !! 0 /= "-n" = exitWith84error
    | args !! 2 /= "-l" = exitWith84error
    | args !! 4 /= "-f" = exitWith84error
    | otherwise = return ()

checkNbArgs :: [String] -> IO ()
checkNbArgs args
    | length args /= 6 && length args /= 1 ||
    length args == 0 = exitWith84error
    | length args == 1 && args !! 0 /= "-h" = exitWith84error
    | length args == 1 && args !! 0 == "-h" = putStrLn (usage True)
    | otherwise = return ()

checkDataConf :: Conf -> IO ()
checkDataConf conf
    | isNothing (nbcolors  conf) = exitWith84error
    | isNothing (cvglimit conf) = exitWith84error
    | otherwise = return ()

checkLengthString :: String -> IO ()
checkLengthString str
    | length str == 0 = exitWith84error
    | otherwise = return ()

checkLengthFromList :: Int -> IO ()
checkLengthFromList n
    | isEven n == True = exitWith84error
    | otherwise = return ()
