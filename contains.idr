import Data.So
import Data.List.Quantifiers
import Data.String.Views
import Data.List

allowedWords : List String
allowedWords = [ "foo", "bar", "baz" ] 

data StrPrefix : StrList outer -> StrList inner -> Type where
  Empty : StrPrefix (SCons x xs) SNil   
  Next : StrPrefix ys xs -> StrPrefix (SCons x ys) (SCons x xs) 

prefixWorks : StrPrefix (strList "foo12312") (strList "foo") 
prefixWorks = Next (Next (Next Empty)) 

anyDoesNot : Any ( \inner => StrPrefix (strList "bar1234") (strList inner) ) Main.allowedWords
--anyDoesNot = There (Here (Next (Next (Next Empty))))
anyDoesNot = There (Here (Next (Next (Next Empty))))

infixes : List String
infixes = filter (\x => isInfixOf x "bar1234") Main.allowedWords 

numberInfixes : String -> Nat
numberInfixes word = length $ filter (\w => isInfixOf w word) Main.allowedWords


isGT : GT (length Main.infixes) Z 
isGT = LTESucc LTEZero

print : (w:String) -> { auto prf : GT (numberInfixes w) Z } -> String
print w = w

x : String
x = print "bax1234"


anyRefl : Any ( \inner => (=) "bar" inner) Main.allowedWords
anyRefl = There (Here Refl)

