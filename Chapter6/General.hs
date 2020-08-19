{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE InstanceSigs        #-}
module General
  ( General.id,
    applyToFive
  )
where

------------------------------------------------------------------------------

-- | Haskell crimes
-- 
-- applyToFive :: (a -> a) -> Int
-- applyToFive f = f 5
--
-- applyToFive :: forall a . (a -> a) -> Int
-- applyToFive f = f 5

------------------------------------------------------------------------------
id :: forall a . a -> a
id a = a

applyToFive :: (forall a . a -> a) -> Int
applyToFive f = f 5

------------------------------------------------------------------------------
newtype Cont a = Cont
  { unCont :: forall r . (a -> r) -> r
  }

------------------------------------------------------------------------------

-- | Exercise 6.4-i
-- Provide a `Functor` instance for `Cont`. Hint: Use lots of typed holes, and
-- an explicit lambda whenever looking for a function type.
instance Functor Cont where
  fmap :: (a -> b) -> Cont a -> Cont b
  -- f :: a -> b
  -- unCont contA :: (a -> r) -> r
  -- fmap f (Cont a) = Cont $ \y -> y . a $ f -- home-grown
  fmap f contA = Cont $ flip (. unCont contA) f -- brought to you by pointfree.io

------------------------------------------------------------------------------

-- | Exercise 6.4-ii
-- Provide the `Applicative` instances for `Cont`
instance Applicative Cont where
  pure :: a -> Cont a
  -- x :: a
  -- y :: a -> r
  pure x = Cont (\y -> y x)
  --
  (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  -- x :: ((a -> b) -> r) -> r
  -- y :: (a -> r) -> r
  -- Cont x <*> Cont y = Cont $ (\z -> z $ y (\_ -> x y)) -- ':<
  Cont x <*> Cont y = Cont ($ y (const (x y)))
