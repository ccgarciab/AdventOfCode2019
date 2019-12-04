calcSimpleFuel :: Integer -> Integer
calcSimpleFuel n = (n `div` 3) - 2

calcFuel :: Integer -> Integer
calcFuel = sum . tail . takeWhile (>0) . iterate calcSimpleFuel

main :: IO ()
main = interact $ (++"\n") . show . sum . map calcFuel . map read . lines
