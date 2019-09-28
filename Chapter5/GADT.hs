
{-# LANGUAGE GADTs, LambdaCase #-}

data Expr a where
  LitInt  :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool

  If  :: Expr Bool -> Expr a -> Expr a -> Expr a
  Add :: Expr Int -> Expr Int -> Expr Int
  Not :: Expr Bool -> Expr Bool


evalExpr :: Expr a -> a
evalExpr = \case
  LitInt  i -> i
  LitBool b -> b
  If s p r  ->
    case evalExpr s of
      True  -> evalExpr p
      False -> evalExpr r
  Add i i'  -> evalExpr i + evalExpr i'
  Not b  -> not $ evalExpr b

data Expr_ a
  = (a ~ Int)  => LitInt_ Int
  | (a ~ Bool) => LitBool_ Bool
  | (a ~ Int)  => Add_ (Expr_ Int) (Expr_ Int)
  | (a ~ Bool) => Not_ (Expr_ Bool)
  | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

