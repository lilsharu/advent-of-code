import System.Environment (getArgs)
import Text.Read (readMaybe)

newtype Range = Range (Int, Int)

-- | Based off of the `words` Prelude
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen c str = case dropWhile c str of
  "" -> []
  str' -> w : wordsWhen c str''
    where
      (w, str'') = break c str'

parseRange :: String -> Maybe Range
parseRange s = case wordsWhen (== '-') s of
  [start, end] -> do
    a <- readMaybe start
    b <- readMaybe end
    pure (Range (a, b))
  _ -> Nothing

splitByLength :: Int -> [a] -> [[a]]
splitByLength len xs
  | len <= 0 = error "splitByLength: length must be positive"
  | null xs = []
  | otherwise = take len xs : splitByLength len (drop len xs)

allPartsMatch :: Int -> Int -> Bool
allPartsMatch n k =
  let s = show n
   in length (show n) `rem` k == 0
        && case splitByLength k s of
          [] -> True
          (x : xs) -> all (== x) xs

isInvalidId :: Int -> Bool
isInvalidId n =
  let d = length (show n)
   in any
        (allPartsMatch n)
        [1 .. d `div` 2]

getInvalidIds :: Range -> [Int]
getInvalidIds (Range (start, end)) =
  filter isInvalidId [start .. end]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      contents <- readFile filename
      case traverse parseRange (wordsWhen (== ',') contents) of
        Nothing -> putStrLn "Invalid Input File"
        Just ranges -> do
          let total = sum (map (sum . getInvalidIds) ranges)
          putStrLn ("Sum: " ++ show total)
    _ -> putStrLn "Usage: day-02a <filename>"
