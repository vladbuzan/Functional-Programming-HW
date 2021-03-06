-----------------------
-- Vlad-Sebastian Buzan
-- 28.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

import Data.List

strWords :: String -> [String]
strWords str = 
  if (str == []) then [] else
    let
      current = takeWhile (\x -> x /= ' ') str
    in
    case current of
      [] -> strWords (dropWhile (\x -> x == ' ') str)
      _ -> current : strWords (dropWhile (\x -> x /= ' ') str)

piglatinize :: String -> String
piglatinize str = 
  let
    words = strWords str
    
    pigLatVowel word = 
      word ++ "-hay"
    
    pigLatConsonant word =
      let
        c = take 1 word
        rest = drop 1 word
      in
        rest ++ "-" ++ c ++ "ay"

    pigLatWord word = 
      if (elem (head word) ['A', 'E', 'I', 'O', 'U', 'a', 'e', 'i', 'o', 'u']) 
        then pigLatVowel word
      else 
        pigLatConsonant word

  in
  intercalate " " (map (\x -> pigLatWord x) words)

iter :: (a -> a) -> a -> [a]
iter f x = x : map f (iter f x)

apprPi :: Double
apprPi = 
  let
    nextPi :: Double -> Double
    nextPi currentPi = 
      currentPi + (2 * cos (currentPi / 2)) / (2 * sin (currentPi / 2) - 1)
    
    a = iter nextPi 0
  in
  last (takeWhile (\x -> (x - (nextPi x)) /= 0) a)


-- Implement:

-- the update function for option 1

update :: (Eq k) => (v -> v) -> v -> k -> [(k, v)] -> [(k, v)]
update fn def k l =
  let
    l1 = takeWhile (\x -> fst x /= k) l
    l2 = dropWhile (\x -> fst x /= k) l

    updateVal (x, y) = 
      (x, fn y)

  in
  case l2 of
    [] -> l ++ [(k, def)]
    _  -> l1 ++ ((updateVal (head l2)) : (tail l2)) 


-- OR

-- the uniques, countOccurrences and countWords functions for option 2

uniques :: (Eq a) => [a] -> [a]
uniques _ = error "Implement this function"

countOccurrences :: (Eq a) => a -> [a] -> Int
countOccurrences _ _ = error "Implement this function"

countWords :: String -> [(String, Int)]
countWords _ = error "Implement this function"

-- Implement topWords using the functions implemented above.

topWords :: Int -> String -> [(String, Int)]
topWords 0 _ = []
topWords _ [] = []
topWords n str = 
  let
    words = strWords str
    
    helper listWords acc =
      case listWords of
        [] -> acc
        x:xs -> helper xs (update (+1) 1 x acc)

    sort (x1, y1) (x2, y2) 
      | y1 > y2 = LT
      | y1 < y2 = GT
      | y1 == y2 = compare x1 x2
        
  in
  take n (sortBy (sort) (helper words []))

