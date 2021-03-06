(*
-----------------------
-- Vlad-Sebastian Buzan
-- 28.11.2020
-----------------------
-- Edit the lines above with your name and the submission date.
*)

fun levenshtein (s1 : string) (s2 : string): int =
  let
    val a1 = String.explode s1
    val a2 = String.explode s2
  
    fun lev (ar1 : char list) (ar2 : char list) =
      if (ar1 = []) then 
        if (ar2 = []) then 0
        else length ar2
      else if (ar2 = []) then length ar1 else 
        if (hd ar1 = hd ar2) then lev (tl ar1) (tl ar2)
            else 1 + Int.min((lev (tl ar1) ar2), Int.min((lev ar1 (tl ar2)),             lev (tl ar1) (tl ar2)))
      
  in
  lev a1 a2 
end;