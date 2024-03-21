{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- ErrorHandling
-}
    
module ErrorHandling (
    errorHandling
) where

import Data.Maybe (isNothing)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import GetArgs
import Lib
import Error
import Usage

-- check si le nombre de pos de listOfTuples est egal au nombre du truples de listofTruples sinon exit84error
-- check les string avec length et comparer les length

data List = List {
    listOfTruples :: Maybe String,
    listOfTuples :: Maybe String
} deriving Show

defaultList :: List
defaultList = List {
    listOfTruples = Nothing,
    listOfTuples = Nothing
}

splitStr :: String -> [String]
splitStr [] = []
splitStr (x:xs)
    | x `elem` " " = splitStr xs
    | otherwise = tuple : splitStr rest
    where
        (tuple, rest) = break (`elem` " ") (x:xs)

countNumbers :: String -> Int
countNumbers = length.filter (== ',')

updateTuples :: String -> List -> IO List
updateTuples str list = 
    let currentTuples = fromMaybe "" (listOfTuples list)
        updatedTuples = if null currentTuples then str
        else currentTuples ++ "," ++ str
    in return $ list { listOfTuples = Just updatedTuples }

updateTruples :: String -> List -> IO List
updateTruples str list = 
    let currentTruples = fromMaybe "" (listOfTruples list)
        updatedTruples = if null currentTruples then str
        else currentTruples ++ "," ++ str
    in return $ list { listOfTruples = Just updatedTruples }

updateList :: String -> List -> IO List
updateList str list
    | countNumbers str == 1 = updateTuples str list
    | countNumbers str == 2 = updateTruples str list
    | otherwise = return list

updateListRecursive :: [String] -> List -> IO List
updateListRecursive [] list = return list
updateListRecursive (x:xs) list = do
    updatedList <- updateList x list
    updateListRecursive xs updatedList

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
    let test = unwords contentFileTolines
    print contentFileTolines
    print test
    let content = splitStr test
    print content
    finalList <- updateListRecursive content defaultList
    print finalList
    -- rajouter le check pour savoir si les 2 listes son égales

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
