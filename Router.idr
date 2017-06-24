import Data.Vect
import Data.HVect


data Literal : Type where
  ValidLiteral : (chars:List Char) -> Literal
  InvalidLiteral : (invalid:Char) -> Literal
  End : Literal

implementation Show Literal where
  show (ValidLiteral chars) = pack chars
  show (InvalidLiteral c) = show c
  show End = ""

literalChars : List Char
literalChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

toLiteral: (xs: List Char) -> Literal
toLiteral [] = End
toLiteral (c :: chars) = if elem c literalChars
                           then case toLiteral chars of
                                     ValidLiteral s => ValidLiteral $ [c] ++ s
                                     InvalidLiteral c => InvalidLiteral c
                                     End => ValidLiteral $ [c]
                           else InvalidLiteral c

data StaticRoute : Literal -> Type where
  MkStaticRoute : (lit:Literal) -> StaticRoute lit


Lit : (route:String) -> StaticRoute $ toLiteral $ unpack route
Lit route = MkStaticRoute $ toLiteral $ unpack $ route


GET : StaticRoute (ValidLiteral _) -> String
GET (MkStaticRoute v @ (ValidLiteral chars)) = show v
