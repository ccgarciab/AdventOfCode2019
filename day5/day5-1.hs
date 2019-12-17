import qualified Data.Map.Strict as M
    
type Program = M.Map Integer Integer

data Computer = Computer { program :: Program
                         , input   :: [Integer]
                         , output  :: [Integer]
                         , iptr    :: Integer
                         } deriving (Show)

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
                                                   [(i, 3) | i<-[1, 2]] ++
                                                   [(99, 0)]
    ms  = map f $ drop 2 $ reverse $ show n
    len = arity - (length ms)
    f x
        | x == '0' = Position
        | x == '1' = Inmediate
    modes x
        | x `elem` [5, 6] = ms ++ (replicate len Position)
        | x `elem` [1..8] = ms ++ (replicate (len - 1) Position) ++ [Inmediate]
        | x == 99         = []


argValues :: Computer -> Integer -> [Mode] -> Maybe [Integer]
argValues c ar modes = do
    args <- takeFromProg c ar
    sequence $ (map f $ zip args modes)
  where
    f (a, m)
        | m == Inmediate = Just a
        | m == Position  = M.lookup a $ program c
        | otherwise      = Nothing


runOp :: Computer -> Integer -> [Integer] -> Maybe Computer
runOp c 99 _           = Just c
runOp c 1 (a:b:pos:[]) = Just $ insertInProg c (a + b) pos
runOp c 2 (a:b:pos:[]) = Just $ insertInProg c (a * b) pos
runOp c 3 (pos:[])     = do
    (pC, val) <- popInput c
    return $ insertInProg pC val pos
runOp c 4 (pos:[])     = do
    val <- M.lookup pos (program c)
    return $ pushOut c val
runOp _ _ _            = Nothing


run :: Computer -> Maybe Computer
run c = do
    rawOp   <- M.lookup (iptr c) (program c)
    let (op, modes, arity) = parseOp rawOp
    argvals <- argValues c arity modes
    nextC   <- runOp c op argvals
    case op of
        99 -> Just c
        _  -> run $ nextC{iptr= arity + 1 + (iptr nextC)}


main = do
    rawInput <- getLine
    let inp = map read $ splitBy ',' rawInput
    let prog  = M.fromList $ zip [0..] inp
    let comp  = Computer{program=prog, input=[1], output=[], iptr=0}
    print $ run comp

