module SeoWordsProvider

import Data.String

-- Asks nicely for the user to supply the size of C's size_t type on this
-- machine
--export
readLines : String -> List String
readLines content = Strings.split (\c => List.elem c ['\n', '\r']) content

-- readFile : (filepath : String) -> IO (Either FileError String)
export
readSeoWords : String -> IO $ Provider $ List String
readSeoWords fileName = do maybeFile <- readFile fileName
                           pure $ case maybeFile of 
                                       Left error => Error $ "Failed to load seo words: " ++ show error
                                       Right content => Provide (readLines content) 


{-
getSizeT : IO (Provider Int)
getSizeT = do
  putStrLn "I'm sorry, I don't know how big size_t is. Can you tell me, in bytes?"
  resp <- getLine
  case parsePositive {a=Int} resp of
       Just sizeTSize => pure (Provide sizeTSize)
-}
