import Data.List
import Data.HVect


allowedChars : List Char
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

maxLevel : Nat
maxLevel = 3 

data ValidString : List Char -> Type where
  One : { auto prf : Elem value Main.allowedChars } -> ValidString [value]
  Cons : { auto prf : Elem value Main.allowedChars } -> ValidString xs -> ValidString (value :: xs) 

data LiteralRoute : String -> Type where
  --Literal : (lit : String) -> { auto prf : ValidString (unpack lit) } -> LiteralRoute lit
  LiteralR : { auto prf : ValidString (unpack lit) } -> LiteralRoute lit
                    
Literal : (lit:String) -> { auto prf : LiteralRoute lit } -> LiteralRoute lit
Literal lit {prf} = prf

data Base = MkBase    

infixl 9 /
data Route :  Vect k Type -> Type where
  Root : Route [Base]
  (/) : (parent : Route segment) -> (child : c) -> Route (c :: segment)
--  Slash : (parent : Route segment) -> (child : c) -> Route (c :: segment)

data IsSubRoute : Route pS -> Route cS -> Type where
  Parent : (parent : Route segment) -> (child : Route (c :: segment)) -> IsSubRoute parent child
--  Parent : (parent : Route segment) -> (child : Route (c :: segment)) -> IsSubRoute parent child

implicit toLiteral : (lit:String) -> { auto prf : ValidString (unpack lit) } -> LiteralRoute lit
toLiteral lit {prf} = Literal lit

--(/) : (parent : Route segment) -> (child : c) -> Route (c :: segment)
--(/) parent child = Slash parent child

--x : Route [Base,LiteralRoute "category",LiteralRoute "fashion"]
--x = Root / "category" / "fashion"

infixr 10 &
data RoutesConfiguration : Type where
  Empty : (route: Route [Base]) -> RoutesConfiguration
  (&) : (parent: Route parentSegment) -> 
  --Routes : (parent: Route parentSegment) -> 
           (route : Route (childSegment :: parentSegment)) -> 
--           { auto is : IsSubRoute parent route } -> 
           { auto lt : LTE (length (childSegment :: parentSegment) ) Main.maxLevel } -> 
           RoutesConfiguration

test : RoutesConfiguration
test = (Root / Literal "category") & 
       (Root / Literal "category" / Literal "bla")  -- / Literal "fashion" / Literal "bla" ) 

