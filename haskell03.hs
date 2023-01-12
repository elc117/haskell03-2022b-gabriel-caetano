add10toall :: [Int] -> [Int]
add10toall list = [x + 10 | x <- list]

multN :: Int -> [Int] -> [Int]
multN n list = [x * n | x <- list]

multN' :: Int -> [Int] -> [Int]
multN' n list = map (\x -> x * n) list

applyExpr :: [Int] -> [Int]
applyExpr list = [3 * x + 2 | x <- list]

applyExpr' :: [Int] -> [Int]
applyExpr' list = map (\x -> 3 * x + 2) list

addSuffix :: String -> [String] -> [String]
addSuffix sulf list = [x ++ sulf | x <- list]

selectgt5 :: [Int] -> [Int]
selectgt5 list = [x | x <- list, x > 5]

sumOdds :: [Int] -> Int
sumOdds list = foldl (+) 0 [x | x <- list, odd x]

sumOdds' :: [Int] -> Int
sumOdds' list = foldl (+) 0 (filter (\x -> odd x) list)

selectExpr :: [Int] -> [Int]
selectExpr list = [x | x <- list, x >= 20 && x <= 50]

countShorts :: [String] -> Int
countShorts words = length [x | x <- words, length x < 5]

calcExpr :: [Float] -> [Float]
calcExpr list = [x ^ 2 / 2 | x <- list]

trSpaces :: String -> String
trSpaces str = [if x == ' ' then '-' else x | x <- str]

selectSnd :: [(Int,Int)] -> [Int]
selectSnd tuples = [snd tuple | tuple <- tuples]

dotProd :: [Int] -> [Int] -> Int
dotProd li1 li2 = foldl (+) 0 [x * y | (x, y) <- zip li1 li2]