{-# LANGUAGE InstanceSigs #-}

module Chapter3 where

newtype T1 a = T1 (Int -> a)

newtype T2 a = T2 (a -> Int)

newtype T3 a = T3 (a -> a)

newtype T4 a = T4 ((Int -> a) -> Int)

newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T1 where
  fmap :: (a -> b) -> T1 a -> T1 b
  fmap f (T1 a) = T1 $ f <$> a

instance Functor T5 where
  fmap :: (a -> b) -> T5 a -> T5 b
  fmap f (T5 a) = T5 $ \x -> a $ flip (.) f x


