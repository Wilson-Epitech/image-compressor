module Main (main) where
import Lib

distance:: (Int, Int, Int) -> (Int, Int, Int) -> Float
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