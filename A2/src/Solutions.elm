-----------------------
-- Buzan Vlad-Sebastian
-- 21.10.2020
-----------------------
-- Edit the lines above with your name and the submission date.
module Solutions exposing (..)
import Debug exposing (log)
import List exposing (append, reverse, sort, take)
import String exposing (toList)

type Face = Ace | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT
            | NINE | TEN | Jack | Queen | King

type Suit = Clubs | Diamonds | Hearts | Spades

type Card = Card Face Suit

deck : List Card
deck =
    let
        enumSuit = [Clubs, Diamonds, Hearts, Spades]
        enumFace = [Ace, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT,
            NINE, TEN,Jack, Queen, King]

        iterateFaces: (List Face) -> Suit -> (List Card)
        iterateFaces listFaces suit =
            case listFaces of
                [] -> []
                x::xs -> (Card x suit) :: (iterateFaces xs suit)

        iterateSuits: (List Suit) -> (List Card)
        iterateSuits listSuits =
            case listSuits of
                [] -> []
                x::xs -> append (iterateFaces enumFace x) (iterateSuits xs)
    in
    iterateSuits enumSuit


cardValue : Card -> List Int
cardValue (Card face _) =
    case face of
        Ace -> [1, 11]
        TWO -> [2]
        THREE -> [3]
        FOUR -> [4]
        FIVE -> [5]
        SIX -> [6]
        SEVEN -> [7]
        EIGHT -> [8]
        NINE -> [9]
        TEN -> [10]
        Jack -> [10]
        Queen -> [10]
        King -> [10]

smallestK : (List comparable) -> Int -> (List comparable)
smallestK list k =
    take k (sort list)

balanced : String -> Bool
balanced str =
    let
        list = toList(str)

        isBalanced : (List Char) -> Int -> Bool
        isBalanced string openParantheses =
            case string of
                [] -> if openParantheses == 0 then True else False
                x::xs -> if x == '(' then isBalanced xs (openParantheses + 1)
                         else if x == ')' then if openParantheses == 0 then False
                                            else isBalanced xs (openParantheses - 1)
                              else isBalanced xs openParantheses
    in
    isBalanced list 0

coinChange : Int -> (List Int) -> Int
coinChange value list =
    let
        sortedList = reverse (sort list)

        helper : Int -> (List Int) -> Int
        helper left list1 =
            case list1 of
                [] -> if left == 0 then 1 else 0
                x::xs -> if left == 0 then 1
                         else if left < 0
                                then 0
                              else (helper (left - x) list1 + helper left xs)

    in
    helper value sortedList