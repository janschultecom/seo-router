import Data.List
import Data.HVect


allowedChars : List Char
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

maxLevel : Nat
maxLevel = 3 

data ValidLiteral : List Char -> Type where
  One : { auto prf : Elem value Main.allowedChars } -> ValidLiteral [value]
  Multi : { auto prf : Elem value Main.allowedChars } -> ValidLiteral xs -> ValidLiteral (value :: xs) 

data LiteralRoute : String -> Type where
  LiteralR : { auto prf : ValidLiteral (unpack lit) } -> LiteralRoute lit
                    
Literal : (lit:String) -> { auto prf : LiteralRoute lit } -> LiteralRoute lit
Literal lit {prf} = prf

data Base = MkBase    

infixl 9 /
data Route :  Vect k Type -> Type where
  Root : Route [Base]
  (/) : (parent : Route segment) -> (child : c) -> Route (c :: segment)

implicit toLiteral : (lit:String) -> { auto prf : ValidLiteral (unpack lit) } -> LiteralRoute lit
toLiteral lit {prf} = Literal lit

--x : Route [Base,LiteralRoute "category",LiteralRoute "fashion"]
--x = Root / "category" / "fashion"

infixr 8 &
data RoutesConfiguration : Type where
  Empty : (route: Route [Base]) -> RoutesConfiguration
  (&) : (parent: Route parentSegment) -> 
           (route : Route (childSegment :: parentSegment)) -> 
           { auto lt : LTE (length (childSegment :: parentSegment) ) Main.maxLevel } -> 
           RoutesConfiguration

data HttpMethod = GET | POST | DELETE | PUT 

test : RoutesConfiguration
test = Root / Literal "category" & 
       Root / Literal "category" / Literal "bla"  -- / Literal "fashion" / Literal "bla" ) 

