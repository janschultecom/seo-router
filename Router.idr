import Data.Vect
import Data.HVect

data SeoChar2 : Type where
    MkSeoChar2 : (c:Char) -> SeoChar2 

isCharAllowed : Char -> Bool
isCharAllowed c = let allowedChars = ['a'..'z'] ++ ['-'] in
                      elem c allowedChars
           

char2Seo : (c:Char) -> { auto ok : isCharAllowed c = True } -> SeoChar2 
char2Seo x = MkSeoChar2 x





