import Data.List
import Data.HVect

allowedChars : List Char
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

data ValidLiteral : List Char -> Type where
  One : { auto prf : Elem value Main.allowedChars } -> ValidLiteral [value]
  Multi : { auto prf : Elem value Main.allowedChars } -> ValidLiteral xs -> ValidLiteral (value :: xs) 

data LiteralRoute : String -> Type where
  Literal : (lit:String) -> { auto prf : ValidLiteral (unpack lit) } -> LiteralRoute lit
                    
data Base = MkBase    

maxLevel : Nat
maxLevel = 3 

infixl 9 /
data Route :  List Type -> Type where
  Root : Route [Base]
  (/) : Route parent -> 
        child ->
        { auto prf : LTE (length (child :: parent) ) Main.maxLevel } ->  
        Route (child :: parent)

-- route : Route [Base,LiteralRoute "category",LiteralRoute "fashion"]
-- route = Root / "category" / "fashion"

--------------------------------------

data HttpMethod = HttpGet | HttpPost | HttpDelete | HttpPut 

HttpHandler : Type 
HttpHandler = String -> String 

RouteHandler : List Type -> Type
RouteHandler r = (HttpMethod, Route r, HttpHandler)

GET : Route r -> HttpHandler -> RouteHandler r 
GET route handler = (HttpGet, route, handler) 

handler : HttpHandler
handler = id

sampleRoute : (HttpMethod, Route [LiteralRoute "category", Base], HttpHandler )
sampleRoute = GET (Root / Literal "category") handler

---------------------------------------

infixl 8 &
data RoutesConfiguration : List (List Type) -> Type where
  Routes : (root: RouteHandler [Base]) -> RoutesConfiguration [[Base]]
  (&) : RoutesConfiguration routes -> 
        RouteHandler (child :: parent)  ->
        { auto prf : Elem parent routes } ->  
        RoutesConfiguration ( (child :: parent) :: routes) 

data HttpService : Type where
  MkHttpService : RoutesConfiguration (x :: xs) -> HttpService 

routes : HttpService
routes = MkHttpService $ Routes ( GET Root handler ) &      
          GET (Root / Literal "category") handler & 
          GET (Root / Literal "category" / Literal "bla") handler 
          

