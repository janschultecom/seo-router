import Data.Vect
import Data.HVect

data SeoChar : Char -> Type where
  A : SeoChar 'a'
  B : SeoChar 'b'
  C : SeoChar 'c'
  Dash : SeoChar '-'

test : SeoChar 'a' -> SeoChar 'b'
test A = B

fromChar : (c:Char) -> SeoChar c 
fromChar 'a' = A
fromChar 'b' = B
fromChar 'c' = C
fromChar '-' = Dash

list : HVect [SeoChar 'a']
list = (::) A [] 

charToVect: (c:Char) -> HVect [SeoChar c]
charToVect c = [fromChar c]

charVect: (c:Char) -> HVect xs -> HVect (SeoChar c :: xs)
charVect c vect = fromChar c :: vect

--charsToVect: Vect k Char -> Type
--charsToVect [] = HVect []
--charsToVect (c :: cs) = let x = charVect c  cs in
--                            ?whsd
charsToVect: List Char -> Type 
charsToVect [] = HVect []
charsToVect (c :: cs) = HVect [SeoChar c, charsToVect cs] 


parse : (chars: List Char) -> charsToVect chars
parse [] = []
parse (c :: cs) = let x = parse cs
                      y = fromChar c
                      z = x :: Nil 
                      in
                      y :: z 

x : List Char
x = ['a','b','-']

y : List Char
y = ['a','b','z']

