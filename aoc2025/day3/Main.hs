import Data.List (elemIndex)

-- Find indices of all list elements equal to given
findAll :: (Eq a) => a -> [a] -> [Int]
findAll x xs = map fst $ filter ((== x) . snd) $ zip [0..] xs

-- Calculate maximum number of length n constructed of digits xs
maxN :: Int -> String -> Integer
maxN 1 xs = read [(maximum xs)]
maxN n xs =
    let available = (take (length xs - n + 1) xs)
        first = (maximum available)
        allFirst = findAll first available
        combinations = map (\x -> maxN (n - 1) (drop (x + 1) xs)) allFirst
    in
        ((read [first]) * 10^(n-1)) + maximum combinations

part1 :: IO ()
part1 = do
    input <- readFile "input.txt"
    let banks = lines input
        joltage = sum $ map (maxN 2) banks
    print joltage

part2 :: IO ()
part2 = do
    input <- readFile "input.txt"
    let banks = lines input
        joltage = sum $ map (maxN 12) banks
    print joltage
