import Data.List


allowedChars : List Char
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']


validElem : (value : a) -> (xs : List a) ->
             {auto prf : Elem value xs} ->
             a
validElem value xs = value

c : Char
c = validElem 'a' allowedChars

-- will not compile
-- y: Char
-- y = validElem '^' allowedChars

data ValidString : (allowedChars: List Char) -> List Char -> Type where
  One : { allowedChars : List Char } -> { auto prf : Elem value allowedChars } -> ValidString allowedChars [value]
  Cons : { allowedChars : List Char } -> { auto prf : Elem value allowedChars } -> ValidString allowedChars xs -> ValidString allowedChars (value :: xs) 

Literal : {allowedChars : List Char} -> (literal : List Char) -> { auto prf : ValidString allowedChars literal } -> ValidString allowedChars literal
Literal {allowedChars} [value] {prf = One} = One
Literal {allowedChars} (value :: xs) {prf = (Cons there)} = Cons there

x : List Char
x = unpack "abc$-"

y : ValidString Main.allowedChars Main.x
y = Literal Main.x



