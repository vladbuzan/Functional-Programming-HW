-----------------------
-- Buzan Vlad-Sebastian
-- 21.10.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Regex exposing (..)
import Debug exposing (log)
import List exposing (drop, append)
import Tuple exposing (first)

type RegexPattern
  = Literal Char
  | Many RegexPattern
  | OneOf RegexPattern RegexPattern
  | Seq RegexPattern RegexPattern

{-
  The `Ok` variant represents the matched input and the rest of the unmatched input
  The `Err` variant represents the original input
-}
type alias RegexResult = Result (List Char) (List Char, List Char)

{-
  Returns the `Ok` variant if the literal character matches the first character of the string.
  If the string is empty or the characters don't match the `Err` variant should be returned.
  ```elm
  matchLit 'a' ['a', 'b', 'b'] == Ok (['a'], ['b', 'b'])
  matchLit 'c' ['a', 'b', 'b'] == Err ['a', 'b', 'b']
  matchLit 'c' [] == Err []
  ```
-}
matchLit : Char -> List Char -> RegexResult
matchLit ch str =
    case str of
        [] -> Err str
        x::xs -> if x == ch then Ok ([ch], xs) else Err str
{-
  Matches `pat1` and then `pat2`. Returns `Ok` only if both succeed.
  ```elm
  matchSeq (Literal 'a') (Literal 'b') ['a', 'b', 'c'] == Ok (['a', 'b'], ['c'])
  matchSeq (Literal 'a') (Literal 'b') ['a', 'x', 'c'] == Err (['a', 'x', 'c'])
  matchSeq (Seq (Literal 'a') (Literal 'b')) (Literal 'c') ['a', 'b', 'c', 'd'] == Ok (['a', 'b', 'c'], ['d'])
  ```
-}
matchSeq : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchSeq pat1 pat2 input =
    let
        res1 = match pat1 input
    in
    case res1 of
        Ok (matched, unmatched) -> let res2 = match pat2 unmatched in
                                   case res2 of
                                       Ok (matched1, unmatched1) -> Ok (append matched matched1, unmatched1)
                                       Err _ -> Err input
        Err _ -> Err input
{-
  Matches the pattern `pattern` zero or many times. Always returns the `Ok` variant.
  ```elm
  matchMany (Literal 'a') ['a', 'a', 'a'] == Ok (['a', 'a', 'a'], [])
  matchMany (Literal 'b') ['a', 'a', 'a'] == Ok ([], ['a', 'a', 'a'])
  matchMany (Literal 'b') ['b', 'b', 'a'] == Ok (['b', 'b'], ['a'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'b', 'a', 'c'] == Ok (['b', 'a', 'b', 'a'], ['c'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'c', 'a', 'c'] == Ok (['b', 'a'], ['c', 'a', 'c'])
  ```
-}
matchMany : RegexPattern -> List Char -> RegexResult
matchMany pattern input =
    let
        res = match pattern input
    in
    case res of
        Ok (matched, unmatched) -> let res1  = matchMany pattern unmatched in
                                   case res1 of
                                       Ok (matched1, unmatched1) -> Ok ((append matched matched1), unmatched1)
                                       Err _ -> Ok (matched, unmatched)
        Err unmatched -> Ok ([], unmatched)

{-
  Tries to match one of `pat1` and `pat2`, in this order. If `pat1` matches, its result is returned, else
  `pat2` is tried.
  ```elm
  matchOneOf (Literal 'a') (Literal 'b') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matchOneOf (Literal 'b') (Literal 'a') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matcmatchhOneOf (Seq (Literal 'a') (Literal 'b')) (Seq (Literal 'c') (Literal 'd'))) ['c', 'd', 'a'] (Ok (['c', 'd'], ['a']))
  ```
-}
matchOneOf : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchOneOf pat1 pat2 input =
    let
        res = match pat1 input
    in
    case res of
        Ok _ -> res
        Err _ -> match pat2 input


match : RegexPattern -> List Char -> RegexResult
match pattern input =
  case pattern of
    Literal char -> matchLit char input
    Many pat -> matchMany pat input
    OneOf pat1 pat2 -> matchOneOf pat1 pat2 input
    Seq pat1 pat2 -> matchSeq pat1 pat2 input