import Data.List
import Data.HVect


allowedChars : List Char
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

data ValidString : List Char -> Type where
  One : { auto prf : Elem value Main.allowedChars } -> ValidString [value]
  Cons : { auto prf : Elem value Main.allowedChars } -> ValidString xs -> ValidString (value :: xs) 

data LiteralRoute : (lit:String) -> Type where
  Literal : (lit : String) -> { auto prf : ValidString (unpack lit) } -> LiteralRoute lit
                    
data Base = MkBase    

data Route :  Vect k Type -> Type where
  Root : Route [Base]
  Slash : (parent : Route segment) -> (child : c) -> Route (segment ++ [c])

data SubRoute : Route p -> Route c -> Type where
  Parent : (parent : Route segment) -> (child : Route (segment ++ [c])) -> SubRoute parent child

implicit toLiteral : (lit:String) -> { auto prf : ValidString (unpack lit) } -> LiteralRoute lit
toLiteral lit {prf} = Literal lit

-- ($) :: (a -> b) -> a -> b  
(/) : (parent : Route segment) -> (child : c) -> Route (segment ++ [c])
(/) parent child = Slash parent child

--x : Route [Base,LiteralRoute "category",LiteralRoute "fashion"]
--x = Root / "category" / "fashion"

data RoutesConfiguration : Type where
  Routes : (route : Route segment) -> RoutesConfiguration

test : RoutesConfiguration
test = Routes ( Root / Literal "category" / Literal "fashion" ) 
