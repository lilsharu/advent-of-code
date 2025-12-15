import Data.List (foldl')
import System.Environment (getArgs)

data Direction = R Int | L Int

lockSize :: Int
lockSize = 100

startIndex :: Int
startIndex = 50

parse :: String -> Maybe Direction
parse s =
  case s of
    ('L' : rest)
      | [(n, "")] <- reads rest ->
          Just (L n)
    ('R' : rest)
      | [(n, "")] <- reads rest ->
          Just (R n)
    _ -> Nothing

applyTurn :: Int -> Direction -> Int
applyTurn start turn =
  case turn of
    L times -> (start - times) `mod` lockSize
    R times -> (start + times) `mod` lockSize

additiveModularInverse :: (Integral a) => a -> a -> a
additiveModularInverse a n = (-(a `mod` n)) `mod` n

-- | Counts the number of times the dial passes zero when turned from a given
-- starting point by a certain number of ticks.
countZeroPasses :: Int -> Direction -> Int
countZeroPasses start turn =
  case turn of
    L times -> ((additiveModularInverse start lockSize) + times) `div` lockSize
    R times -> (start + times) `div` lockSize

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let inputLines = filter (not . null) (lines contents)
      let maybeParsedLines = traverse parse inputLines
      case maybeParsedLines of
        Nothing -> putStrLn "Invalid Input File"
        Just parsedLines -> do
          let reduceFn (position, count) direction = (applyTurn position direction, count + countZeroPasses position direction)
          let reduced = foldl' reduceFn (startIndex, 0) parsedLines
          let count = snd reduced
          putStrLn ("Count: " ++ show count)
    _ -> putStrLn "Usage: day-01b <filename>"
