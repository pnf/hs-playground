module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sayHi :: String -> IO ()
sayHi x = putStrLn ("Hello" ++ x ++ "!")

foo x =
   let y = x * 2
   in y + 1
