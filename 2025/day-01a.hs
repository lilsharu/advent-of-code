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

applyTurn :: Int -> Maybe Direction -> Int
applyTurn start turn =
  case turn of
    Just (L times) -> mod (start - times) lockSize
    Just (R times) -> mod (start + times) lockSize
    Nothing -> start

countZeros :: [Int] -> Int
countZeros = length . filter (== 0)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      let lines' = lines contents
      let parsed_lines = map parse lines'
      let positions = tail $ scanl applyTurn startIndex parsed_lines
      let count = countZeros positions
      putStrLn ("Count: " ++ show count)
    _ -> putStrLn "Usage: day-01a <filename>"
