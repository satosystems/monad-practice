module Main where

import Lib

list1 :: List Int
list1 = Cons 1 $ Cons 2 $ Cons 3 $ Nil

list2 :: List (Int -> Int)
list2 = Cons (+ 1) $ Cons (+ 10) Nil

func1 :: Int -> List String
func1 n = Cons (show n) Nil

func2 :: String -> List String
func2 s = Cons ("(" ++ s ++ ")") Nil

func3 :: List a -> [a]
func3 Nil = []
func3 (Cons x t) = x:func3 t

func4 :: [a] -> List a
func4 [] = Nil
func4 (x:xs) = Cons x $ func4 xs

main :: IO ()
main = do
  print $ show ((+ 1) <$> list1) == "Cons 2 (Cons 3 (Cons 4 Nil))"
  print $ show (list2 <*> list1) == "Cons 2 (Cons 3 (Cons 4 (Cons 11 (Cons 12 (Cons 13 Nil)))))"
  print $ show (list1 >>= func1) == "Cons \"1\" (Cons \"2\" (Cons \"3\" Nil))"
  print $ (return 100 >>= func1) == func1 100
  print $ (list1 >>= return) == list1
  print $ ((list1 >>= func1) >>= func2) == (list1 >>= (\x -> func1 x >>= func2))
  print $ ((Cons 200 Nil) >>= return) == Cons 200 Nil
  print $ ((Cons 300 Nil >>= func1) >>= func2) == (Cons 300 Nil >>= (\x -> func1 x >>= func2))

