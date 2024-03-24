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

-- error handling: file exist ?
--faire un try/catch pour savoir si le file exsist

main :: IO ()
main = do
    args <- getArgs
    errorHandling args
    list <- parseInput args
    print (listOfTruples list)
    print (listOfTuples list)