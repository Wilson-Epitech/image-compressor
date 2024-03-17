{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Main
-}

module Main (main) where
import ErrorHandling

-- error handling: file exist ?

-- main :: IO ()
-- main = do
--     args <- getArgs
--     checkNbArgs args
--     checkOrderArgs args
--     let conf = getOpts defaultConf args
--     checkNegativeArgs conf
--     checkDataConf conf
--     contentFile <- readFile (args !! 5)
--     let contentFileTolines = lines contentFile
--     let linesWithCommas = convertToString contentFileTolines
--     checkLengthString linesWithCommas
--     let updatedList = defaultList {listOfTruples = Just linesWithCommas}
--     putStrLn (fromJust (listOfTruples updatedList))

main :: IO ()
main = do
    errorHandling