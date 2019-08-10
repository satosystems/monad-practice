module Lib where

infixr 6 :::

data List a = Nil | (:::) a (List a) deriving Eq

instance Show a => Show (List a) where
  show x = "[" ++ showList' x ++ "]"

showList' :: Show a => List a -> String
showList' Nil = ""
showList' (x ::: Nil) = show x
showList' (x ::: xs) = show x ++ "," ++ showList' xs

instance Functor List where
  fmap f Nil = Nil
  fmap f (x ::: l) = (f x) ::: (fmap f l)

infixr 5 +++

(+++) :: List a -> List a -> List a
Nil +++ l = l
l +++ Nil = l
x ::: Nil +++ l = x ::: l
x ::: t +++ l = x ::: (t +++ l)

instance Applicative List where
  pure x = x ::: Nil
  Nil <*> _ = Nil
  f ::: fs <*> m = (f <$> m) +++ (fs <*> m)

instance Monad List where
  Nil >>= f = Nil
  x ::: t >>= f = f x +++ (t >>= f)
  return = pure
