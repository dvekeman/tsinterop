module Lib where

import Text.Parsec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

parseFromFile :: Parsec String () a -> FilePath -> IO (Either ParseError a)
parseFromFile p fname
    = do{ input <- readFile fname
        ; return (runParser p () fname input)
        }