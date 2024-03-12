module Main (main) where
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Lib

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance (r1, g1, b1) (r2, g2, b2) =
    let r = fromIntegral (r1 - r2)^2
        g = fromIntegral (g1 - g2)^2
        b = fromIntegral (b1 - b2)^2
        distanceSquared = r + g + b
    in sqrt distanceSquared

main :: IO ()
main = do
    let computeDistance = distance (3,4,5) (3,8,2)
    print computeDistance
    args <- getArgs
    print args
    print (args !! 1)
    if null args
        then exitWith (ExitFailure 84)
        else do
            content <- readFile (head args)
            putStrLn content
