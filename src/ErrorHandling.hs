{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- ErrorHandling
-}
    
module ErrorHandling (
    errorHandling,
    checkLengthString,
    checkLengthFromList
) where

import Data.Maybe (isNothing)
import GetArgs
import Lib
import Error
import Usage

errorHandling :: [String] -> IO ()
errorHandling args =
    checkNbArgs args >>
    checkOrderArgs args >>
    let conf = getOpts defaultConf args
    in checkNegativeArgs conf >>
    checkDataConf conf

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
    | null str = exitWith84error
    | otherwise = return ()

checkLengthFromList :: Int -> IO ()
checkLengthFromList n
    | isEven n == True = exitWith84error
    | otherwise = return ()
