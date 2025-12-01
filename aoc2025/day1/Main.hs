-- Command for rotation to the Left/Right by given distance
type Command = Either Int Int

-- Parse command from string
-- L99 -> Left 99
-- R45 -> Right 99
parseCommand :: String -> Command
parseCommand (d:s)
    | d == 'L' = Left (read s)
    | d == 'R' = Right (read s)
    | otherwise = error "Could not parse command"

-- Rotate arrow from given number using given command
rotate :: Int -> Command -> Int
rotate num (Left distance) = (num - distance) `mod` 100
rotate num (Right distance) = (num + distance) `mod` 100

-- Rotate arrow from given number using given command, and return tuple of (zero count, new number)
-- We count only final zero
rotateCountFinal :: (Int, Int) -> Command -> (Int, Int)
rotateCountFinal (cnt, num) cmd = (cnt', next)
    where next = rotate num cmd
          cnt' | next == 0 = succ cnt
               | otherwise = cnt

-- Rotate arrow from given number using given command, and return tuple of (zero count, new number)
-- We count all zeros (final and intermediate)
rotateCountIntermediate :: (Int, Int) -> Command -> (Int, Int)
rotateCountIntermediate (cnt, num) (Left distance) =
    let nextDenorm = num - distance
        intermediateZeroes = abs (nextDenorm `div` 100) - (if num == 0 then 1 else 0)
        next = nextDenorm `mod` 100
        cnt' = cnt + intermediateZeroes + (if next == 0 then 1 else 0)
    in (cnt', next)

rotateCountIntermediate (cnt, num) (Right distance) =
    let nextDenorm = num + distance
        intermediateZeroes = nextDenorm `div` 100
        next = nextDenorm `mod` 100
        cnt' = cnt + intermediateZeroes
    in (cnt', next)

-- Rotate arrow from given number using given command and count rotations
execute :: [Command] -> Int -> Int
execute cmds start = fst $ foldl rotateCountFinal (0, 50) cmds

executeIntermediate :: [Command] -> Int -> Int
executeIntermediate cmds start = fst $ foldl rotateCountIntermediate (0, 50) cmds

-- Part 1
part1 :: IO ()
part1 = do
  input <- readFile "input.txt"
  let commands = map parseCommand $ lines $ input
  let finale = execute commands 50
  print finale

-- Part 2
part2 :: IO ()
part2 = do
  input <- readFile "input.txt"
  let commands = map parseCommand $ lines $ input
  let finale = executeIntermediate commands 50
  print finale
