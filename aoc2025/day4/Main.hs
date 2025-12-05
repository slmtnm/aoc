data Point = Point Int Int deriving (Show, Eq)

adjacent :: Int -> Int -> Point -> [Point]
adjacent w h (Point x y) = [ Point (x + dx) (y + dy)
                           | dx <- [-1, 0, 1]
                           , dy <- [-1, 0, 1]
                           , x + dx >= 0
                           , x + dx < w
                           , y + dy >= 0
                           , y + dy < h
                           , dx /= 0 || dy /= 0]

removable :: [String] -> [Point]
removable m = filter available $ filter (\(Point x y) -> m !! y !! x == '@') [Point x y | x <- [0..(w-1)], y <- [0..(h-1)]]
    where h = length m
          w = length (head m)
          available p = (<4) $ length $ filter (== '@') $ map (\(Point x y) -> m !! y !! x) $ adjacent w h p

removableIterate :: Int -> [String] -> Int
removableIterate acc m =
    let
        h = length m
        w = length (head m)
        points = removable m
        removedCount = length points
        newMap = [ [ if (Point x y) `elem` points then '.' else m !! y !! x | x <- [0..(w-1)] ] | y <- [0..(h-1)] ]
    in 
        if removedCount == 0 then acc else removableIterate (acc + removedCount) newMap

part1 :: IO ()
part1 = do
    input <- readFile "input.txt"
    let rows = lines input
    print $ length $ removable rows

part2 :: IO ()
part2 = do
    input <- readFile "input.txt"
    let rows = lines input
    print $ removableIterate 0 rows
