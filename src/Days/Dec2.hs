module Days.Dec2 where

test :: String
test = "test"

splitBy :: String -> Char -> (String, String)
splitBy input c = (str1, dropWhile (== c) str2)
    where (str1, str2) = span (/= c) input

split :: String -> Maybe (String, String)
split str = 
    if (even . length) str then
    Just (splitAt mid str)
    else Nothing
    where mid = length str `div` 2

findRanges :: String -> [String]
findRanges []    = []
findRanges input = range : findRanges input'
    where (range, input') = splitBy input ','

parseRange :: String -> (Integer, Integer)
parseRange input = (read str1, read str2)
    where (str1, str2) = splitBy input '-'

isValid :: Integer -> Bool
isValid int = case (split . show) int of
    Just (str1, str2) -> str1 /= str2
    Nothing           -> True

checkInvalidNumber :: Integer -> Maybe Integer
checkInvalidNumber int = if isValid int then
                         Nothing
                         else Just int

findInvalidInRange :: (Integer, Integer) -> [Integer]
findInvalidInRange (start, end) = filter (not . isValid) [start..end]

findInvalidSum :: String -> Integer
findInvalidSum = sum . findInvalidInRange . parseRange
