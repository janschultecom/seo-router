module Main

import SeoWordsProvider
import Data.List

%language TypeProviders

%provide (seoWords : List String) with readSeoWords "seo-words.txt"

say : (validWords : List String) -> (s:String) -> { auto prf : Elem s validWords } -> IO ()
say _ s = printLn s

main : IO ()
main = say seoWords "sportssada"
