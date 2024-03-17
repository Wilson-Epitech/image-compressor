{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Error
-}

module Error ( 
    exitWith84error
) where

import System.Exit (exitWith, ExitCode(ExitFailure))

exitWith84error :: IO ()
exitWith84error = exitWith (ExitFailure 84)
