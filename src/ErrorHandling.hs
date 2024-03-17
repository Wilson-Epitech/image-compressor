{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Main
-}
    
module ErrorHandling
    ( errorHandling
    ) where

import Text.Read (readMaybe)
import Data.Maybe (isNothing)
import Data.List (intercalate)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import System.Exit (exitWith, ExitCode(ExitFailure))

data Conf = Conf {
    nbcolors :: Maybe Int,
    cvglimit :: Maybe Float,
    filepath :: Maybe String
} deriving Show

data List = List {
    listOfTruples :: Maybe String
}

errorHandling :: IO ()
errorHandling = do
    args <- getArgs
    checkNbArgs args
    checkOrderArgs args
    let conf = getOpts defaultConf args
    checkNegativeArgs conf
    checkDataConf conf
    contentFile <- readFile (args !! 5)
    let contentFileTolines = lines contentFile
    let linesWithCommas = convertToString contentFileTolines
    checkLengthString linesWithCommas
    let updatedList = defaultList {listOfTruples = Just linesWithCommas}
    putStrLn (fromJust (listOfTruples updatedList))


defaultConf :: Conf
defaultConf = Conf {
    nbcolors = Nothing,
    cvglimit = Nothing,
    filepath = Nothing
}

defaultList :: List
defaultList = List {
    listOfTruples = Nothing
}

exitWith84error :: IO ()
exitWith84error = exitWith (ExitFailure 84)

convertToString :: [String] -> String
convertToString = intercalate ","

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

getOpts :: Conf -> [String] -> Conf
getOpts conf [] = conf
getOpts conf ("-n":value:rest)
    = getOpts conf {nbcolors = readMaybe value} rest
getOpts conf ("-l":value:rest)
    = getOpts conf {cvglimit = readMaybe value} rest
getOpts conf ("-f":value:rest)
    = getOpts conf {filepath = Just value} rest
getOpts conf _ = conf

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance (r1, g1, b1) (r2, g2, b2) =
    let r = fromIntegral (r1 - r2)^(2 :: Int)
        g = fromIntegral (g1 - g2)^(2 :: Int)
        b = fromIntegral (b1 - b2)^(2 :: Int)
        distanceSquared = r + g + b
    in sqrt distanceSquared

usage :: Bool -> String
usage True =
    "USAGE: ./imageCompressor -n N -l L -f F\n\n" ++
    "      N       number of colors in the final image\n" ++
    "      L       convergence limit\n" ++
    "      F       path to the file containing the colors of the pixels\n"
usage False = []