{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- GetArgs
-}

module GetArgs( 
    Conf(..),
    defaultConf,
    getOpts,
) where

import Text.Read (readMaybe)

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

getOpts :: Conf -> [String] -> Conf
getOpts conf [] = conf
getOpts conf ("-n":value:rest)
    = getOpts conf {nbcolors = readMaybe value} rest
getOpts conf ("-l":value:rest)
    = getOpts conf {cvglimit = readMaybe value} rest
getOpts conf ("-f":value:rest)
    = getOpts conf {filepath = Just value} rest
getOpts conf _ = conf