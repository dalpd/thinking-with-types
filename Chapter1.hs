module Chapter1 where

data Spin = Up | Down

-- since type Spin has the same cardinality as Bool, they must be isomorphic

boolToSpin1 :: Bool -> Spin
boolToSpin1 False = Up
boolToSpin1 True  = Down

spinToBool1 :: Spin -> Bool
spinToBool1 Up   = False
spinToBool1 Down = True

-- for two types with cardinality ~ n there are n! unique isomorphisms

boolToSpin2 :: Bool -> Spin
boolToSpin2 False = Down
boolToSpin2 True  = Up

spinToBool2 :: Spin -> Bool
spinToBool2 Up   = True
spinToBool2 Down = False

-- | Exercise 1.2-i
-- Determine the cardinality of Either Bool (Bool, Maybe Bool) -> Bool
-- s1_2 :: Either Bool (Bool, Maybe Bool) -> Bool
--
-- 2 + (2 * (2 + 1)) -> 2
-- |a -> b| = |b| ^ |a|
-- 2 ^ 8 = 256

