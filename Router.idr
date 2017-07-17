import Data.Vect
import Data.HVect


literalChars : List Char
literalChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

Error : Type
Error = String

Literal : Type
Literal = List Char

toLiteralHelper : (c:Char) -> (cs:List Char) -> Either Error Literal
toLiteralHelper c Nil = if elem c literalChars
                            then Right [c]
                            else Left ("Found invalid char " ++ (show c))
toLiteralHelper c (c' :: chars) = if elem c literalChars
                                     then case toLiteralHelper c' chars of
                                               Right validLiteral => Right $ c :: validLiteral
                                               left => left
                                     else Left $ "Found invalid char " ++ (show c)


toLiteral: (xs: List Char) -> Either Error Literal
toLiteral [] = Left "Literal cannot be empty"
toLiteral (c :: chars) = toLiteralHelper c chars

data StaticRoute : Either Error Literal -> Type where
  MkStaticRoute : (lit: Either Error Literal) -> StaticRoute lit


Lit : (route:String) -> StaticRoute $ toLiteral $ unpack route
Lit route = MkStaticRoute $ toLiteral $ unpack $ route

GET : StaticRoute $ Right _ -> String
GET (MkStaticRoute (Right chars)) = pack chars

p1 : HVect [Char]
p1 = ['c']

p2 : HVect [Int]
p2 = [2]

c1 : HVect [Char, Int]
c1 = ['c',1]


IsChild : {t : Type } -> (parent:HVect ts) -> (child: HVect (ts ++ [t])) -> HVect (ts ++ [t])
IsChild parent child = child
