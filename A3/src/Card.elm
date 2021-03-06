-----------------------
-- Vlad-Sebastian Buzan
-- 09.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Card exposing (Card(..), Face(..), Suit(..), cardValue, viewCard, cardToString, deck)

import Html exposing (..)
import Html.Attributes exposing (style)
import List exposing (append, reverse, sort, take)
import String exposing (toList)
{-
  Replace with your definitions from assignment 1
-}
type Face = Ace | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT
            | NINE | TEN | Jack | Queen | King

type Suit = Clubs | Diamonds | Hearts | Spades

type Card = Card Face Suit

faceToString : Face -> String
faceToString face = case face of
        Ace -> "Ace"
        TWO -> "Two"
        THREE -> "Three"
        FOUR -> "Four"
        FIVE -> "Five"
        SIX -> "Six"
        SEVEN -> "Seven"
        EIGHT -> "Eight"
        NINE -> "Nine"
        TEN -> "Ten"
        Jack -> "Jack"
        Queen -> "Queen"
        King -> "King"

suitToString : Suit -> String
suitToString suit = case suit of
    Clubs -> "Clubs"
    Diamonds -> "Diamonds"
    Hearts -> "Hearts"
    Spades -> "Spades"

cardToString : Card -> String
cardToString (Card face suit) = faceToString face ++ " of " ++ suitToString suit

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

deck : List Card
deck = let
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

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode (Card face suit) =
   case face of
     Ace -> case suit of
       Spades ->"🂡"
       Hearts -> "🂱"
       Clubs ->  "🃑"
       Diamonds -> "🃁"
     TWO -> case suit of
       Spades ->"🂢"
       Hearts -> "🂲"
       Clubs ->  "🃒"
       Diamonds -> "🃂"
     THREE -> case suit of
       Spades ->"🂣"
       Hearts -> "🂳"
       Clubs ->  "🃓"
       Diamonds ->"🃃"
     FOUR -> case suit of
       Spades ->"🂤"
       Hearts -> "🂴"
       Clubs ->  "🃔"
       Diamonds -> "🃄"
     FIVE -> case suit of
       Spades ->"🂥"
       Hearts -> "🂵"
       Clubs ->  "🃕"
       Diamonds -> "🃅"
     SIX -> case suit of
       Spades ->"🂦"
       Hearts -> "🂶"
       Clubs ->  "🃖"
       Diamonds -> "🃆"
     SEVEN -> case suit of
       Spades ->"🂧"
       Hearts -> "🂷"
       Clubs ->  "🃗"
       Diamonds -> "🃇"
     EIGHT -> case suit of
       Spades -> "🂨"
       Hearts ->  "🂸"
       Clubs ->   "🃘"
       Diamonds ->  "🃈"
     NINE -> case suit of
       Spades -> "🂩"
       Hearts ->  "🂹"
       Clubs ->   "🃙"
       Diamonds ->  "🃉"
     TEN -> case suit of
       Spades ->"🂪"
       Hearts -> "🂺"
       Clubs ->  "🃚"
       Diamonds -> "🃊"
     Jack -> case suit of
       Spades ->"🂫"
       Hearts -> "🂻"
       Clubs ->  "🃛"
       Diamonds -> "🃋"
     Queen -> case suit of
       Spades ->"🂭"
       Hearts -> "🂽"
       Clubs ->  "🃝"
       Diamonds -> "🃍"
     King -> case suit of
       Spades -> "🂮"
       Hearts -> "🂾"
       Clubs ->  "🃞"
       Diamonds -> "🃎"


{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard (Card face suit) =
   let
     faceName = faceToString face
     suitName = suitToString suit
     suitColor s =
       case s of
         Diamonds -> "red"
         Spades -> "black"
         Hearts -> "red"
         Clubs -> "black"
     unicode = cardToUnicode (Card face suit)
   in
     div [style "display" "inline-block"] [
       div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
       div [style "font-size" "0.8em"]  [text (cardToString (Card face suit))]
     ]