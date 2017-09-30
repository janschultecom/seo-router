
import Data.Vect

data CopK : Type where
  Nil : CopK
  (::) : (head : Type -> Type) -> (tail : CopK) -> CopK

data ElemK : (value : (Type -> Type)) -> CopK -> Type where
  Here : ElemK head (head :: tail)
  There : (later : ElemK x xs) -> ElemK x ( y :: xs) 

foo : CopK
foo = List :: (Vect 3) :: Maybe :: Nil

x : ElemK Maybe Main.foo 
x = There (There Here)
