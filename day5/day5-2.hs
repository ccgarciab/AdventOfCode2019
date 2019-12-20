import qualified Data.Map.Strict as M

type Program = M.Map Integer Integer

data Computer = Computer { program :: Program
                         , input   :: [Integer]
                         , output  :: [Integer]
                         , iptr    :: Integer
                         } deriving (Show, Read)

data Mode = Position | Inmediate deriving (Show, Eq)


splitBy :: (Foldable t, Eq p) => p -> t p -> [[p]]
splitBy delimiter = foldr f [[]] 
  where
    f c l@(x:xs) | c == delimiter = []:l
                 | otherwise = (c:x):xs


popInput :: Computer -> Maybe (Computer, Integer)
popInput c = do
    n      <- if null $ input c then Nothing else Just $ head $ input c
    pInput <- if null $ input c then Nothing else Just $ tail $ input c
    return (c{input= pInput}, n)    


pushOut :: Computer -> Integer -> Computer
pushOut c out = c{output= out:(output c)}


insertInProg :: Computer -> Integer -> Integer -> Computer
insertInProg c elem pos = c{program= M.insert pos elem (program c)}


takeFromProg :: Computer -> Integer -> Maybe [Integer]
takeFromProg c n = sequence $ map (\i -> M.lookup (i + (iptr c)) $ program c) [1..n]


parseOp :: Integer -> (Integer, [Mode], Integer)
parseOp n = (op, modes op, fromIntegral arity)
  where
    op  = n `mod` 100
    arity  = snd $ head $ filter ((== op) . fst) $ [(i, 1) | i<-[3, 4]] ++ 
                                                   [(i, 2) | i<-[5, 6]] ++ 
                                                   [(i, 3) | i<-[1, 2, 7, 8]] ++ 
                                                   [(99, 0)]
    ms  = map f $ drop 2 $ reverse $ show n
    len = arity - (length ms)
    f x
        | x == '0' = Position
        | x == '1' = Inmediate
    modes op'
        | op' == 4           = if null ms then [Position] else [Inmediate]
        | op' `elem` [3, 99] = []
        | op' `elem` [1..8]  = (take 2 $ ms ++ [Position, Position]) ++ [Inmediate]


runOp :: Computer -> Integer -> [Integer] -> [Mode] -> Maybe Computer
runOp c op args modes = case op of
    1 -> do
         (a:b:pos:[]) <- values
         return $ insertInProg c (a + b) pos
    2 -> do
         (a:b:pos:[]) <- values
         return $ insertInProg c (a * b) pos
    3 -> do
         (new_c, val) <- popInput c
         return $ insertInProg new_c val $ head args
    4 -> do
         let [mode] = modes
         [val]   <- values
         return $ pushOut c val
    5 -> do
         (b:p:[]) <- values
         return $ c{iptr= if b == 0 then iptr c else p - 3}
    6 -> do
         (b:p:[]) <- values
         return $ c{iptr= if b == 0 then p - 3 else iptr c}
    7 -> do
         (a:b:pos:[]) <- values
         return $ insertInProg c (if a < b  then 1 else 0) pos
    8 -> do
         (a:b:pos:[]) <- values
         return $ insertInProg c (if a == b  then 1 else 0) pos
    99 -> Just c
    _  -> Nothing
  where
    select arg mode
        | mode == Position = M.lookup arg $ program c
        | otherwise        = Just arg
    values = sequence $ zipWith select args modes


run :: Computer -> Maybe Computer
run c = do
    rawOp   <- M.lookup (iptr c) (program c)
    let (op, modes, arity) = parseOp rawOp
    args    <- takeFromProg c arity
    nextC   <- runOp c op args modes
    case op of
        99 -> Just c
        _  -> run $ nextC{iptr= arity + 1 + (iptr nextC)}


main = do
    rawInput <- getLine
    let inp = map read $ splitBy ',' rawInput
    let prog  = M.fromList $ zip [0..] inp
    let comp  = Computer{program=prog, input=[5], output=[], iptr=0}
    print $ run comp
