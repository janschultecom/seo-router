import Data.List
import Data.HVect


allowedChars : List Char
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

data ValidString : List Char -> Type where
  One : { auto prf : Elem value Main.allowedChars } -> ValidString [value]
  Cons : { auto prf : Elem value Main.allowedChars } -> ValidString xs -> ValidString (value :: xs) 

Literal : (literal : List Char) -> { auto prf : ValidString literal } -> ValidString literal
Literal [value] {prf = One} = One
Literal (value :: xs) {prf = (Cons there)} = Cons there


data Route : Type where
 Lit : (lit : ValidString x) ->  Route

data IsSubRoute : HVect ts -> Type where
  Root : IsSubRoute []
  Slash : (parent : ts ) -> (child : t ) -> IsSubRoute [ts ++ t]


x : IsSubRoute [String]
x = Slash Root ["asd"]
