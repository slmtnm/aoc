import Data.Function ((&))

-- Splits string str by delimiter delim
split delim str =
    case p2 of
        [] -> [p1]
        (_ : next) -> p1 : split delim next
    where (p1, p2) = span (/= delim) str


-- isInvalid2 returns true if given number conists of two repeating parts
-- like "123123"
isInvalid2 :: Int -> Bool
isInvalid2 number = 
    p1 == p2
    where snum = show number
          len = length snum
          (p1, p2) = splitAt (len `div` 2) snum

-- isInvalidAny returns true if given number conists of >2 repeating parts
isInvalidAny :: Int -> Bool
isInvalidAny number = 
    number `elem` possibleNumbers
    where possibleLengths = [i | i <- [1..len `div` 2], len `mod` i == 0]
          possibleNumbers = map (read :: String -> Int) $ map (\l -> take len $ cycle (take l snum)) possibleLengths
          snum = show number
          len = length snum
          (p1, p2) = splitAt (len `div` 2) snum


-- Sums up all invalid IDs in range [start, end]
sumInvalid invalidFunc [start, end] =
    sum $ filter invalidFunc [istart..iend]
    where istart = read start :: Int
          iend = read end :: Int

main :: IO ()
main = do
    input <- readFile "input.txt"
    let ranges = input & filter (/= '\n') & split ',' & map (split '-')
        solution1 = ranges & map (sumInvalid isInvalid2) & sum
        solution2 = ranges & map (sumInvalid isInvalidAny) & sum
    putStrLn $ "Part1: " ++ (show solution1)
    putStrLn $ "Part2: " ++ (show solution2)
