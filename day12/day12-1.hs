import Data.Function

data Moon = Moon { x    :: Int
                 , y    :: Int
                 , z    :: Int
                 , vx   :: Int
                 , vy   :: Int
                 , vz   :: Int
                 , name :: Char
                 } deriving (Show, Read)

instance Eq Moon where
    (==) = (==) `on` name

pull :: Moon -> Moon -> Moon
pull m1 m2 = if m1 == m2 then m1 else m1{vx = f x vx, vy = f y vy, vz = f z vz}
  where
    f coord vel = (vel m1) + (fromEnum $ (compare `on` coord) m2 m1) - 1

gravity :: [Moon] -> [Moon]
gravity moons = [foldl pull m moons | m <- moons]

move :: Moon -> Moon
move m = let f crd vel = (crd m) + (vel m) in m{x = f x vx, y = f y vy, z = f z vz}

velocity :: [Moon] -> [Moon]
velocity = map move

step :: [Moon] -> [Moon]
step = velocity . gravity

energy :: Moon -> Int
energy m = let f = sum . map (abs . ($ m)) in (f [x, y, z]) * (f [vx, vy, vz])

mkmoon :: (Int, Int, Int, Char) -> Moon
mkmoon (x', y', z', n) = Moon {x = x', y = y', z = z', vx = 0, vy = 0, vz = 0, name = n}

input = [ (17, -12, 13, 'A')
        , ( 2,   1,  1, 'B')
        , (-1, -17,  7, 'C')
        , (12, -14, 18, 'D')]

main = print $ sum $ map energy $ head $ drop 1000 $ iterate step $ map mkmoon input
