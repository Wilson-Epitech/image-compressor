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
-- faire un try/catch pour savoir si le fichier existe

getStructArgs :: [String] -> IO ()
getStructArgs args = do
    list <- parseInput args
    let conf = getOpts defaultConf args
        pixels = listOfTruples list
        pos = listOfTuples list
        numberofcluster = maybe 0 id (nbcolors conf)
        minVal = 0
        maxVal = numberofcluster - 1
        convergenceLimit = maybe 0.0 id (cvglimit conf)
    algo pixels pos numberofcluster minVal maxVal convergenceLimit

main :: IO ()
main = do
    args <- getArgs
    errorHandling args
    getStructArgs args
