{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}


module Reverse where



import Data.List

rvrs :: String -> String

rvrs (x:xs) = xs

rvrs2 :: ([x],[x]) -> ([x],[x])
rvrs2 ((x:xs),l2) = rvrs2 (xs, x : l2)
rvrs2 ([],l2) = ([],l2)

main :: IO ()

main = print ()

h :: (Num a, Num b) => a -> b -> b
h = undefined

-- f :: Int -> Int
-- f x = x

jung :: [Int] -> Int -- Ord a => [a] -> a
jung xs = head (sort xs)

blort :: [Int] -> [Int]
blort = sort


-- arith :: Num b => (a -> b) -> Integer -> a -> b
-- arith f i a = (f a) ^ i

class Pet a where
  renamed :: String -> a -> a

newtype Fish = Fish String deriving Show

newtype Cat = Cat String deriving Show

instance Pet Fish where
  renamed s (Fish a) = Fish s

instance Pet Cat where
  renamed s (Cat a) = Cat s

data Collar = forall a. (Pet a,Show a) => Collar a

instance Show Collar where
  show (Collar a) = "Collar " ++ (show a)

pets :: [Collar]
pets = [Collar (Cat "Joe"), Collar(Fish "Jim")]

arith :: Num b => (a -> b) -> Integer -> a -> b
--arith f i j =  if i > 0 then (f j) + (f j) else (f j)
arith f i j =  (f j) + (fromInteger i)

mTh x y z = x * y * z





