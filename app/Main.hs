{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Main
-}

module Main (main) where
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.Maybe (isNothing)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Lib

-- error handling: negatif number

data Conf = Conf {
    nbcolors :: Maybe Int,
    cvglimit :: Maybe Float,
    filepath :: Maybe String
} deriving Show

defaultConf :: Conf
defaultConf = Conf {
    nbcolors = Nothing,
    cvglimit = Nothing,
    filepath = Nothing
}

checkNegativeArgs :: Conf -> IO ()
checkNegativeArgs conf
    | (nbcolors conf) <= Just 0 = exitWith (ExitFailure 84)
    | (cvglimit conf) <= Just 0 = exitWith (ExitFailure 84)
    | otherwise = return ()

checkOrderArgs :: [String] -> IO ()
checkOrderArgs args
    | args !! 0 /= "-n" = exitWith (ExitFailure 84)
    | args !! 2 /= "-l" = exitWith (ExitFailure 84)
    | args !! 4 /= "-f" = exitWith (ExitFailure 84)
    | otherwise = return ()

checkNbArgs :: [String] -> IO ()
checkNbArgs args
    | length args /= 6 && length args /= 1 ||
    length args == 0 = exitWith (ExitFailure 84)
    | length args == 1 && args !! 0 /= "-h" = exitWith (ExitFailure 84)
    | length args == 1 && args !! 0 == "-h" = putStrLn (usage True)
    | otherwise = return ()

checkDataConf :: Conf -> IO ()
checkDataConf conf
    | isNothing (nbcolors  conf) = exitWith (ExitFailure 84)
    | isNothing (cvglimit conf) = exitWith (ExitFailure 84)
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

usage :: Bool -> String
usage True =
    "USAGE: ./imageCompressor -n N -l L -f F\n\n" ++
    "      N       number of colors in the final image\n" ++
    "      L       convergence limit\n" ++
    "      F       path to the file containing the colors of the pixels\n"
usage False = []

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance (r1, g1, b1) (r2, g2, b2) =
    let r = fromIntegral (r1 - r2)^(2 :: Int)
        g = fromIntegral (g1 - g2)^(2 :: Int)
        b = fromIntegral (b1 - b2)^(2 :: Int)
        distanceSquared = r + g + b
    in sqrt distanceSquared

main :: IO ()
main = do
    args <- getArgs
    checkNbArgs args
    checkOrderArgs args
    let conf = getOpts defaultConf args
    checkNegativeArgs conf
    checkDataConf conf
    print conf
    --         content <- readFile (head args)
    --         putStrLn content
