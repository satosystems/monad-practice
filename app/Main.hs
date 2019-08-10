module Main where

import Lib

list1 :: List Int
list1 = 1 ::: 2 ::: 3 ::: Nil

list2 :: List (Int -> Int)
list2 = (+ 1) ::: (+ 10) ::: Nil

func1 :: Int -> List String
func1 n = show n ::: Nil

func2 :: String -> List String
func2 s = ("(" ++ s ++ ")") ::: Nil

func3 :: List a -> [a]
func3 Nil = []
func3 (x ::: t) = x:func3 t

func4 :: [a] -> List a
func4 [] = Nil
func4 (x:xs) = x ::: func4 xs

main :: IO ()
main = do
  print $ show ((+ 1) <$> list1) == "[2,3,4]"
  print $ show (list2 <*> list1) == "[2,3,4,11,12,13]"
  print $ show (list1 >>= func1) == "[\"1\",\"2\",\"3\"]"
  print $ (return 100 >>= func1) == func1 100
  print $ (list1 >>= return) == list1
  print $ ((list1 >>= func1) >>= func2) == (list1 >>= (\x -> func1 x >>= func2))
  print $ ((200 ::: Nil) >>= return) == 200 ::: Nil
  print $ ((300 ::: Nil >>= func1) >>= func2) == (300 ::: Nil >>= (\x -> func1 x >>= func2))
