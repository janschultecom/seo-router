import Data.List
import Data.HVect


allowedChars : List Char
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

maxLevel : Nat
maxLevel = 3 

data ValidString : List Char -> Type where
  One : { auto prf : Elem value Main.allowedChars } -> ValidString [value]
  Cons : { auto prf : Elem value Main.allowedChars } -> ValidString xs -> ValidString (value :: xs) 

data LiteralRoute : (lit:String) -> Type where
  Literal : (lit : String) -> { auto prf : ValidString (unpack lit) } -> LiteralRoute lit
                    
data Base = MkBase    

data Route :  (level:Nat) -> Vect k Type -> Type where
  Root : Route Z [Base]
  Slash : (parent : Route current segment) -> (child : c) -> Route (S current) (segment ++ [c])

data IsSubRoute : Route pL pS -> Route cL cS -> Type where
  Parent : (parent : Route level segment) -> (child : Route (S level) (segment ++ [c])) -> IsSubRoute parent child

implicit toLiteral : (lit:String) -> { auto prf : ValidString (unpack lit) } -> LiteralRoute lit
toLiteral lit {prf} = Literal lit

(/) : (parent : Route current segment) -> (child : c) -> Route (S current) (segment ++ [c])
(/) parent child = Slash parent child

--x : Route [Base,LiteralRoute "category",LiteralRoute "fashion"]
--x = Root / "category" / "fashion"

data RoutesConfiguration : Type where
  Empty : (route: Route Z [Base]) -> RoutesConfiguration
  Routes : (parent: Route parentLevel parentSegment) -> 
           (route : Route childLevel childSegment) -> 
--           { auto is : IsSubRoute parent route } -> 
           { auto lt : LTE childLevel Main.maxLevel } -> 
           RoutesConfiguration

test : RoutesConfiguration
test = Routes Root ( Root / Literal "category" ) -- / Literal "fashion" / Literal "bla" ) 

parent : Route 1 [Base,LiteralRoute "category"]
parent = Root / Literal "category"
