-----------------------
-- Vlad-Sebastian Buzan
-- 10.10.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Solutions exposing (..)

import String exposing (fromFloat)

type Face = Ace | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT
            | NINE | TEN | Jack | Queen | King

type Suit = Clubs | Diamonds | Hearts | Spades

type Card = Card Face Suit

type alias Point = {x: Float, y: Float}

type LineSegment = LineSegment Point Point

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

linesIntersect : LineSegment -> LineSegment -> String
linesIntersect line1 line2 =
    let
        pointToString : Point -> String
        pointToString point =
            "(x: " ++ fromFloat point.x ++ " y: " ++ fromFloat point.y ++ ")"

        slope : LineSegment -> Float --computes slope
        slope (LineSegment point1 point2) =
            ((point2.y - point1.y) / (point2.x - point1.x))

        intersectionPoint : LineSegment -> LineSegment -> Point --computes intersection point
        intersectionPoint lineA lineB =
            let
                (LineSegment pointA _) = lineA --assume y = ax + c and y = bx + d where a and b are the slopes
                (LineSegment pointB _) = lineB
                a = slope lineA
                b = slope lineB
                c = pointA.y - a * pointA.x
                d = pointB.y - b * pointB.x
            in
            Point ((d - c) / (a - b)) (a * ((d - c) / (a - b) + c))

        pointOnLine : Point -> LineSegment -> Bool
        pointOnLine point (LineSegment pointLine1 pointLine2) =
            if (point.x >= pointLine1.x && point.x <= pointLine2.x) || (point.x >= pointLine2.x && point.x <= pointLine1.x) then True
                else False
    in
    if ((slope line1) == (slope line2)) then "Lines are parralel"
        else if (pointOnLine (intersectionPoint line1 line2) line1) && (pointOnLine (intersectionPoint line1 line2) line2) then ("Lines intersect in: " ++ pointToString (intersectionPoint line1 line2))
            else "Lines do not intersect"

trailingZeroes : Int -> Int
trailingZeroes n =
    let
        trailingZeroesHelper : Int -> Int -> Int
        trailingZeroesHelper number power =
            let
                noOfZeroes = number //  power
            in
            if(noOfZeroes == 0) then 0
                else noOfZeroes + trailingZeroesHelper number (power * 5)
    in
    trailingZeroesHelper n 5




