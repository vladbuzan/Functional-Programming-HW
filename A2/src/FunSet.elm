-----------------------
-- Buzan Vlad-Sebastian
-- 21.10.2020
-----------------------
-- Edit the lines above with your name and the submission date.

module FunSet exposing (..)
import List exposing (length, range)
type alias FunSet = Int -> Bool

contains : FunSet -> Int -> Bool
contains set elem = set elem

singletonSet : Int -> FunSet
singletonSet elem = \inputElem -> elem == inputElem

{-
Conveniece function to create a set of elements.
```elm
setOf [1, 2, 3] == union (union (singletonSet 1) (singletonSet 2)) (singletonSet 3))
setOf [1, 2, 3] == fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]
```
-}
setOf : List Int -> FunSet
setOf elems =
    case elems of
        [] -> \_ -> False
        x::xs -> \inputElem -> (inputElem == x) || contains (setOf xs) inputElem

{-
Returns the union of 2 sets.
```elm
(union (singletonSet 1) (singletonSet 2)) 1 == True
(union (singletonSet 1) (singletonSet 2)) 1 == True
```
-}
union : FunSet -> FunSet -> FunSet
union a b = \inputElem -> contains a inputElem || contains b inputElem

{-
Returns the intersection of 2 sets.
```elm
(intersect (setOf [1, 2]) (setOf [1, 3])) 1 == True
(intersect (setOf [1, 2]) (setOf [1, 3])) 2 == False
```
-}
intersect : FunSet -> FunSet -> FunSet
intersect a b = \inputElem -> contains a inputElem && contains b inputElem

{-
Returns the difference of 2 sets.
```elm
(diff (setOf [1, 2]) (setOf [1, 3])) 1 == False
(diff (setOf [1, 2]) (setOf [1, 3])) 2 == True
```
-}
diff : FunSet -> FunSet -> FunSet
diff a b = \inputElem -> contains a inputElem && not (contains b inputElem)

{-
Returns a new set, with `function` applied to each of element.
```elm
(map (\x -> x + 1) (setOf [1, 2]) 1 == False
(map (\x -> x + 1) (setOf [1, 2]) 2 == True
(map (\x -> x + 1) (setOf [1, 2]) 3 == True
```
-}
map: ( Int -> Int ) -> FunSet -> FunSet
map function set =
    let
        values = range (-37) 37

        createSet : (List Int) -> FunSet
        createSet list =
            case list of
                [] -> \_ -> False
                x::xs -> if (contains set x) then union (singletonSet (function x)) (createSet xs)
                         else createSet xs
    in
    createSet values

{-
Takes a list of sets and returns a new set, which is build by applying a fold using `operation` function.
```elm
(fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]) 1 == True
(fold intersection [setOf [1], setOf [2]]) 1 == False
(fold intersection [setOf [1], setOf [2]]) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 1 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 3 == True
```
-}
fold: ( FunSet -> FunSet -> FunSet ) -> List FunSet -> FunSet
fold operation sets =
    case sets of
        [] -> \_ -> False
        x::xs -> if length sets == 1 then x
                 else case xs of
                        [] -> (\_ -> False) --should not get here
                        y::ys -> operation  (operation x y) (fold operation ys)



