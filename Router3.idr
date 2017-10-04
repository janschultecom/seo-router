module Main

import Data.List
import Data.HVect
import WordsProvider

%language TypeProviders
%provide (seoWords : List String) with readWords "seo-words.txt"

numberInfixes : String -> Nat
numberInfixes word = length $ filter (\w => isInfixOf w word) $ filter (/= "") seoWords

allowedChars : List Char
allowedChars = ['a'..'z'] ++ ['0'..'9'] ++ ['-']

data ValidLiteral : List Char -> Type where
  One : { auto prf : Elem value Main.allowedChars } -> ValidLiteral [value]
  Multi : { auto prf : Elem value Main.allowedChars } -> ValidLiteral xs -> ValidLiteral (value :: xs) 

data LiteralRoute : String -> Type where
  Literal : (lit:String) ->
            -- RULE 1  
            { auto prf : ValidLiteral (unpack lit) } -> 
            -- RULE 3
            { auto prf : GT (numberInfixes lit) Z } -> 
            LiteralRoute lit

-----------------------------------
                    
data Base = MkBase    

maxLevel : Nat
maxLevel = 3 

infixl 9 /
data Route :  List Type -> Type where
  Root : Route [Base]
  (/) : Route parent -> 
        child ->
        -- RULE 2
        { auto prf : LTE (length (child :: parent) ) Main.maxLevel } ->  
        Route (child :: parent)

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
        -- RULE 4
        { auto prf : Elem parent routes } ->  
        RoutesConfiguration ( (child :: parent) :: routes) 

data HttpService : Type where
  MkHttpService : RoutesConfiguration (x :: xs) -> HttpService 

routes : HttpService
routes = MkHttpService $ Routes ( GET Root handler ) &      
          GET (Root / Literal "category") handler & 
          GET (Root / Literal "category" / Literal "sports-bar") handler 
          

