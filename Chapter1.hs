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

data TicTacToe a = TicTacToe
  { topLeft   :: a
  , topCenter :: a
  , topRight  :: a
  , midLeft   :: a
  , midCenter :: a
  , midRight  :: a
  , botLeft   :: a
  , botCenter :: a
  , botRight  :: a
  }

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard =
  TicTacToe
    Nothing Nothing Nothing
    Nothing Nothing Nothing
    Nothing Nothing Nothing

data Three = One | Two | Three
  deriving (Eq, Ord, Enum, Bounded)

data TicTacToe2 a = TicTacToe2
  { board :: Three -> Three -> a
  }

emptyBoard2 :: TicTacToe2 (Maybe Bool)
emptyBoard2 =
  TicTacToe2 $ const $ const Nothing

-- | (a ^ b) ^ c = a ^ (b x c)
-- Using Curry-Howard to prove above equality:
-- If one can come up with two functions, one transforming
-- the left side of the equation to the right and another one
-- transforming the right side to the left that means they're isomorphic
-- (c -> b -> a) -> (b, c) -> a
-- ((b, c) -> a) -> c -> b -> a

hurry_coward1 :: (c -> b -> a) -> (b, c) -> a
hurry_coward1 f (v, v') = f v' v

hurry_coward2 :: ((b, c) -> a) -> c -> b -> a
hurry_coward2 f v v' = f $ (,) v' v

-- | (a ^ b) x (a ^ c) = a ^ (b + c)
-- ((b -> a), (c -> a)) -> Either b c -> a
-- Either b c -> a -> ((b -> a), (c -> a))

hurry_coward3 :: ((b -> a), (c -> a)) -> Either b c -> a
hurry_coward3 (f, g) e =
  case e of
    Left  b -> f b
    Right c -> g c

hurry_coward4 :: Either b c -> a -> ((b -> a), (c -> a))
hurry_coward4 _ v = (,) (\_ -> v) (\_ -> v)

-- | (a x b) ^ c = (a ^ c) x (b ^ c)
-- c -> (a, b) ~ ((c -> a), (c -> b))

hurry_coward5 :: c -> (a, b) -> ((c -> a), (c -> b))
hurry_coward5 _ (v', v'') = (,) (\_ -> v') (\_ -> v'')

hurry_coward6 :: ((c -> a), (c -> b)) -> c -> (a, b)
hurry_coward6 (f, g) v = (f v, g v)

