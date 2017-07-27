import Data.Vect
import Data.HVect


literalChars : List Char
literalChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

Error : Type
Error = String

data Literal : (lit:List Char) -> Type where
  MkLiteral : (lit:List Char) -> Literal lit

toLiteral: (xs: List Char) -> Either Error (Literal xs)
toLiteral [] = Right $ MkLiteral [] --Left "Literal cannot be empty"
toLiteral (head :: tail) =
  if elem head literalChars
    then case toLiteral tail of
      Right (MkLiteral _) => Right $ MkLiteral $ head :: tail
      Left msg => Left msg
    else Left $ "Found invalid char " ++ (show head)

data StaticRoute : Either Error (Literal _)-> Type where
  MkStaticRoute : (lit: Either Error (Literal _)) -> StaticRoute lit

LitType : String -> Type
LitType route = StaticRoute $ toLiteral $ unpack route

Lit : (route:String) -> LitType route
Lit route = MkStaticRoute $ toLiteral $ unpack $ route

GET : StaticRoute $ Right _ -> String
GET (MkStaticRoute (Right (MkLiteral lit))) = pack lit

p1 : HVect [Char]
p1 = ['c']

p2 : HVect [Int]
p2 = [2]

c1 : HVect [Char, Int]
c1 = ['c',1]

pr : HVect [LitType "parent"]
pr = [Lit "parent"]

r1 : HVect [LitType "parent", LitType "child"]
r1 = [Lit "parent", Lit "child"]

(/) : (p:String) -> (c:String) -> HVect [LitType p, LitType c]
(/) parent child = [Lit parent, Lit child]

IsChild : {t : Type } -> (parent:HVect ts) -> (child: HVect (ts ++ [t])) -> HVect (ts ++ [t])
IsChild parent child = child
