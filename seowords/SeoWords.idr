module Main

import WordsProvider
import Data.List
import Data.List.Quantifiers

%language TypeProviders

%provide (seoWords : List String) with readWords "seo-words.txt"

numberInfixes : String -> Nat
numberInfixes word = length $ filter (\w => isInfixOf w word) $ filter (/= "") seoWords

say : (s:String) -> { auto prf : GT (numberInfixes s) Z } -> IO ()
say s = printLn s

main : IO ()
main = say "brand-1234"
