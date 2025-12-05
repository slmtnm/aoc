import Data.List (sortOn)

-- Range represents closed range [start, end]
data Range = Range Integer Integer deriving (Show, Eq)

-- Parses range from string "123-456" -> Range 123 456
fromString :: String -> Range
fromString s =
    let (start, end) = span (/= '-') s
    in Range (read start) (read $ tail end)

-- Number of integers in range
rangeAbs :: Range -> Integer
rangeAbs (Range start end) = end - start + 1

-- Checks whether range contains given number
contains :: Range -> Integer -> Bool
contains (Range start end) num  = num >= start && num <= end

-- Checks if given list of ranges contains given number
anyContains :: [Range] -> Integer -> Bool
anyContains [] num = False
anyContains (r:rs) num = r `contains` num || anyContains rs num

-- Merges new range into list of sorted ranges
(|+) :: [Range] -> Range -> [Range]
(|+) [] r2 = [r2]
(|+) rs r2 =
    let r1 = (last rs)
        [(Range start1 end1), (Range start2 end2)] = sortOn (\(Range start _) -> start) [r1, r2]
    in
        if end1 < start2 then rs ++ [r2] else (init rs) ++ [Range start1 (max end1 end2)]

part1 :: IO ()
part1 = do
    input <- lines <$> readFile "input.txt"
    let ranges = map fromString $ takeWhile (/= "") input
    let products = map (read :: String -> Integer) $ tail $ dropWhile (/= "") input
    print $ length $ filter (anyContains ranges) products

part2 :: IO ()
part2 = do
    input <- lines <$> readFile "input.txt"
    let ranges = map fromString $ takeWhile (/= "") input
    let sorted = sortOn (\(Range start end) -> start) ranges
    print $ sum $ map rangeAbs $ foldl (|+) [] sorted
    
