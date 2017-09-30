module Lib where

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

infixr 5 +++

(+++) :: List a -> List a -> List a
Nil +++ l = l
l +++ Nil = l
Cons x Nil +++ l = Cons x l
Cons x t +++ l = Cons x (t +++ l)

instance Functor List where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap f Nil = Nil
  fmap f (Cons x t) = Cons (f x) (fmap f t)

instance Applicative List where
  -- pure :: Applicative f => a -> f a
  -- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
  pure x = Cons x Nil
  Nil <*> _ = Nil
  Cons f fs <*> m = (f <$> m) +++ (fs <*> m)

instance Monad List where
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  -- return :: Monad m => a -> m a
  --
  -- (return x) >>= f == f x
  -- m >>= return == m
  -- (m >>= f) >>= g == m >>= (\x -> f x >>= g)
  Nil >>= f = Nil
  Cons x t >>= f = f x +++ (t >>= f)
  return = pure

