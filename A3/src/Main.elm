-----------------------
-- Vlad-Sebastian Buzan
-- 09.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (..)
import Random
import Debug


import Card exposing (..)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )



type Msg
  = Draw
  | NewCard Card
  | ToogleDeck
  | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->( model
                 , drawCard model
                 )
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( addCardToHand model newCard
      , Cmd.none
      )
    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck -> if model.showDeck then ({model | showDeck = False}, Cmd.none) else ({model | showDeck = True}, Cmd.none)
    Reset -> ({model | deck = (Card.deck), hand = []} ,Cmd.none)

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

addCardToHand : Model -> Card -> Model
addCardToHand model card =
    let
        deck = model.deck
        newDeck = List.filter (\x -> x /= card) deck
    in
    {model | hand = card :: model.hand, deck = newDeck}
{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [Card King Hearts] == 10
  calculateScore [Card Two Hearts] == 2
  calculateScore [Card Two Hearts, Card King Spades] == 12
  calculateScore [Card Ace Hearts, Card King Spades] == 21
  calculateScore [Card Ace Hearts, Card Five Hears, Card Seven Spades] == 13
  calculateScore [Card King Hearts, Card Five Hears, Card Seven Spades] == 22
  calculateScore [Card King Hearts, Card Ten Clubs, Card Ace Spades] == 21
  calculateScore [Card Ace Spades, Card Ace Clubs, Card Ten Clubs, Card King Clubs] == 22
  ```
-}
getCards: List Card -> List (Html msg)
getCards cards =
    List.map (\x -> viewCard x) cards

calculateScore : List Card -> Int
calculateScore cards =
    let
        compareScores s1 s2 =
            case (s1 > 21, s2 > 21) of
                (True, False) -> s2
                (False, True) -> s1
                (False, False) -> if s1 >= s2 then s1 else s2
                (True, True) -> if s1 <= s2 then s1 else s2

        helper acc cardList =
            case cardList of
                [] -> acc
                x::xs -> let
                            val = cardValue x
                         in
                         case val of
                             [] -> acc --should not get here
                             y::ys -> case ys of
                                        []->  helper (acc + y) xs --one value
                                        z::zs -> compareScores (helper (acc + y) xs) (helper (acc + z) xs)
    in
    helper 0 cards


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
    score = calculateScore model.hand
    display = if score == 21 then "Won"
                else if score > 21 then "Lost"
                        else ""
    deck = case model.showDeck of
            True -> div [] (getCards model.deck)
            False -> div [] []
  in
    div [] [ div [] [ h1 [] [text appName] ] ,
        button [onClick ToogleDeck] [text "Toggle"],
        button [onClick Draw, disabled (score >= 21) ] [text "Draw"],
        button [onClick Reset, disabled (score < 21) ] [text "Reset"],
        div [] (getCards model.hand),
        div [] [text (String.fromInt score)],
        div [] [text  display],
        deck
      ]