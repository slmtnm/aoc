import Data.List (findIndex)

---------
-- Part 1
---------

iterateBeam :: Int -> [Bool] -> [Bool] -> [Bool] -> ([Bool], Int)
iterateBeam total acc [] [] = (acc, total)
iterateBeam total acc (b : beam) (s : splitter)
    | b && s =
        let acc' = (init acc) ++ [True, False, True]
            beam' = (drop 1 beam)
            splitter' = (drop 1 splitter)
        in
            iterateBeam (succ total) acc' beam' splitter'
    | otherwise = iterateBeam total (acc ++ [b]) beam splitter

part1 = do
    (first:other) <- lines <$> readFile "input.txt"
    let start = map (== 'S') first
        splitters = map (map (== '^')) other
        solution = snd $ foldl (\(acc, total) line -> iterateBeam total [] acc line) (start, 0) splitters
    print solution

---------
-- Part 2
---------

-- Row represents state of splitters, False = no splitter, True = splitter
type Row = [Bool]

-- Row rank reprsents number of pathes that lead to each cell
type RowRank = [Int]

-- Given row rank, calculate next row rank after going through a row of splitters
tn :: RowRank -> Row -> RowRank
tn rank splitter = go [] rank splitter
    where go acc [] [] = acc
          go acc (r : rank) (s : splitter) =
            if r > 0 && s
                then go ((init acc) ++ [r + last acc, 0, r + (if null rank then 0 else head rank)]) (tail rank) (tail splitter)
                else go (acc ++ [r]) rank splitter

part2 = do
    (first:other) <- lines <$> readFile "input.txt"
    let start = map (\c -> if c == 'S' then 1 else 0) first
        splitters = map (map (== '^')) other
        solution = foldl tn start splitters
    print $ sum solution
