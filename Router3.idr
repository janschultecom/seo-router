import Data.List
import Data.HVect


allowedChars : List Char
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

data ValidString : List Char -> Type where
  One : { auto prf : Elem value Main.allowedChars } -> ValidString [value]
  Cons : { auto prf : Elem value Main.allowedChars } -> ValidString xs -> ValidString (value :: xs) 

{-LiteralT : String -> Type 
LiteralT s = ValidString (unpack s)

LiteralHelper : (lit:List Char) -> ( prf : ValidString lit ) -> ValidString lit
LiteralHelper [value] One = One
LiteralHelper (value :: xs) (Cons x) = Cons x

Literal : (literal : String) -> { auto prf : LiteralT literal } -> LiteralT literal
Literal literal {prf} = let lit = unpack literal in 
                            LiteralHelper lit prf
                         
-}

data LiteralRoute : (lit:String) -> Type where
  Literal : (lit : String) -> { auto prf : ValidString (unpack lit) } -> LiteralRoute lit
                    
data Base = MkBase    

data Route :  Vect k Type -> Type where
  Root : Route [Base]
  Slash : (parent : Route segment) -> (child : c) -> Route (segment ++ [c])

x : Route [Base,LiteralRoute "brands%"]
x = Slash Root (Literal "brands%")

data SubRoute : Route p -> Route c -> Type where
  Parent : (parent : Route segment) -> (child : Route (segment ++ [c])) -> SubRoute parent child

y : SubRoute Root Main.x
y = Parent Root Main.x



-- ($) :: (a -> b) -> a -> b  
{-(/) : (parent : p) -> (child : c) -> Routes
(/) parent child = Slash parent child

x : Routes
x = Root / (Literal "brands") / (Literal "fashion")
-}

