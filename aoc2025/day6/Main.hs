import Data.List (transpose)
import Data.Function ((&))

------- Part 1

-- Takes an operation and executes it on given list of numbers
calc :: Char -> [Int] -> Int
calc '+' = sum
calc '*' = product

-- Takes a list of operations and a list of lists of numbers
execute :: [Char] -> [[Int]] -> [Int]
execute ops numbers = map (uncurry calc) $ zip ops numbers

part1 :: IO ()
part1 = do
    input <- lines <$> readFile "input.txt"
    let numbers = map (map read . words) (init input) :: [[Int]]
        ops = concat $ words $ last input
        solution = sum $ execute ops (transpose numbers)
    print solution

------- Part 2

-- Splits given string on list of indiced characters by any number of whitespace
iwords :: String -> [[(Char, Int)]]
iwords s = helper [] (zip s [0..])
    where
        helper acc pairs = 
            let
                sanitized = dropWhile isCharSpace pairs
                (next, remain) = span (not . isCharSpace) sanitized
            in
                if null next
                    then reverse acc
                    else helper (next : acc) remain
        isCharSpace = (== ' ') . fst

-- Constructs numbers from given indiced lists of characters
icolumns :: [[(Char, Int)]] -> [Int]
icolumns cs =
    let minindex = minimum $ map (snd . head) cs
        maxindex = maximum $ map (snd . last) cs
    in
        helper [] [minindex..maxindex] cs
    where
        helper acc [] _ = reverse acc
        helper acc (ind:indices) cs =
            let numWithInd = cs
                    & map (filter ((== ind) . snd))
                    & filter (not . null)
                    & map (fst . head)
                    & read
            in helper (numWithInd : acc) indices cs

part2 :: IO ()
part2 = do
    input <- lines <$> readFile "input.txt"
    let numbers = map iwords (init input)
        ops = concat $ words $ last input
        solution = sum $ execute ops $ map icolumns $ transpose $ numbers
    print $ solution
