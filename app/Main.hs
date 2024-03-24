{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Main
-}

module Main (main) where
import System.Environment (getArgs)
import ErrorHandling
import Parsing
import Algo
import GetArgs


-- error handling: file exist ?
--faire un try/catch pour savoir si le file exsist

main :: IO ()
main = do
    args <- getArgs
    errorHandling args
    list <- parseInput args
    let conf = getOpts defaultConf args
    algo (listOfTruples list) (listOfTuples list) (maybe 0 id (nbcolors conf)) 0 ((maybe 0 id (nbcolors conf)) - 1) (maybe 0.0 id (cvglimit conf))