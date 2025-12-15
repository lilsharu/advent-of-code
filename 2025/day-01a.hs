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

countZeros :: [Int] -> Int
countZeros = length . filter (== 0)

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
          let positions = tail $ scanl applyTurn startIndex parsedLines
          let count = countZeros positions
          putStrLn ("Count: " ++ show count)
    _ -> putStrLn "Usage: day-01a <filename>"
