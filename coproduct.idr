

import Data.List 

-- Taken from http://nicolas.biri.name/posts/2016-07-26-union-type-in-idris-part-1.html 
data Union : List Type -> Type where
  MemberHere : ty -> Union (ty::ts)
  MemberThere : Union ts -> Union (ty::ts)

member : ty -> {auto e: Elem ty ts} -> Union ts
member x {e = Here} = MemberHere x
member x {e = There later} =
    MemberThere (member x {e = later})

x : Union [String, Nat, List String]
x = member "Ahoy!"

y : Union [String, Nat, List String]
y = member (S Z)




