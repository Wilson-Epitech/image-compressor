{-
-- IMAGE_COMPRESSOR PROJECT, 2024
-- image-compressor
-- File description:
-- Usage
-}

module Usage (
    usage
) where

usage :: Bool -> String
usage True =
    "USAGE: ./imageCompressor -n N -l L -f F\n\n" ++
    "      N       number of colors in the final image\n" ++
    "      L       convergence limit\n" ++
    "      F       path to the file containing the colors of the pixels\n"
usage False = []