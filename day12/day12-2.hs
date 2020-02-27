import Data.Function
                 
data Moonlet = M { c :: Int
                 , v :: Int
                 , n :: Char
                 } deriving (Show, Read, Eq, Ord) 
    
pull :: Moonlet -> Moonlet -> Moonlet
pull m1 m2 = if (n m1) == (n m2) then m1 else m1{v = f c v}
  where
    f coord vel = (vel m1) + (fromEnum $ (compare `on` coord) m2 m1) - 1

gravity :: [Moonlet] -> [Moonlet]
gravity moons = [foldl pull m moons | m <- moons]

move :: Moonlet -> Moonlet
move m = m{c = (c m) + (v m)}

velocity :: [Moonlet] -> [Moonlet]
velocity = map move

step :: [Moonlet] -> [Moonlet]
step = velocity . gravity

mklet :: (Int, Char) -> Moonlet
mklet (co, na) = M{c = co, v = 0, n = na}

mklets :: [Int] -> [Moonlet]
mklets cs = map mklet $ zip cs ['A'..]

cyclen :: [Moonlet] -> Int
cyclen ms = (+ 1) $ length $ takeWhile (/= ms) $ tail $ iterate step ms

xs = [ 17, 2,  -1,  12]
ys = [-12, 1, -17, -14]
zs = [ 13, 1,   7,  18]

main = print $ foldl lcm 1 $ map (cyclen . mklets) [xs, ys, zs]
