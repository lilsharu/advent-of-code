import Data.Bits
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
    Just (Range (a, b))
  _ -> Nothing

topMatchesBottom :: Int -> Int -> Bool
topMatchesBottom n k =
  let val = 10 ^ k
      top = n `div` val
      bottom = n `mod` val
   in top == bottom

isInvalidId :: Int -> Bool
isInvalidId n =
  any (topMatchesBottom n) $
    takeWhile (\k -> 10 ^ (2 * k - 1) < n) [1 ..]

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
          let invalidIds = map getInvalidIds ranges
          print invalidIds
          let totalPerRange = map sum invalidIds
          let total = sum totalPerRange
          putStrLn ("Sum: " ++ show total)
    _ -> putStrLn "Usage: day-02a <filename>"
