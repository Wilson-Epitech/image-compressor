module Main (main) where
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Lib

-- error handling: /4
-- bad arguments
-- ordres des arguments

data Conf = Conf {
    nbcolors :: Maybe Int,
    cvglimit :: Int,
    filepath :: Maybe String
} deriving Show

defaultConf :: Conf
defaultConf = Conf {
    nbcolors = Nothing,
    cvglimit = 0,
    filepath = Nothing
}

checkOrderArgs :: [String] -> IO ()
checkOrderArgs args
    | args !! 0 /= "-n" = exitWith (ExitFailure 84)
    | args !! 2 /= "-l" = exitWith (ExitFailure 84)
    | args !! 4 /= "-f" = exitWith (ExitFailure 84)
    | otherwise = return ()

checkNbArgs :: [String] -> IO ()
checkNbArgs args
    | length args /= 6 && length args /= 1 || length args == 0 = exitWith (ExitFailure 84)
    | length args == 1 && args !! 0 /= "-h" = exitWith (ExitFailure 84)
    | length args == 1 && args !! 0 == "-h" = do 
        let usagestring = usage True
        putStrLn usagestring
    | otherwise = return ()

getOpts :: Conf -> [String] -> Conf
getOpts conf [] = conf
getOpts conf ("-n":value:rest)
    = getOpts conf {nbcolors = Just (read value)} rest
getOpts conf ("-l":value:rest)
    = getOpts conf {cvglimit = (read value)} rest
getOpts conf ("-f":value:rest)
    = getOpts conf {filepath = Just (value)} rest
getOpts conf _ = conf

usage :: Bool -> String
usage True = "USAGE: ./imageCompressor -n N -l L -f F\n\n" ++
            "      N       number of colors in the final image\n" ++
            "      L       convergence limit\n" ++
            "      F       path to the file containing the colors of the pixels\n"
usage False = []

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance (r1, g1, b1) (r2, g2, b2) =
    let r = fromIntegral (r1 - r2)^2
        g = fromIntegral (g1 - g2)^2
        b = fromIntegral (b1 - b2)^2
        distanceSquared = r + g + b
    in sqrt distanceSquared

main :: IO ()
main = do
    args <- getArgs
    checkNbArgs args
    checkNbArgs args
    let conf = getOpts defaultConf args
    print conf
--     -- let computeDistance = distance (3,4,5) (3,8,2)
--     -- print computeDistance
--     --         content <- readFile (head args)
--     --         putStrLn content
