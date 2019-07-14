{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Chapter2 where

import GHC.TypeLits

-- | Show     :: * -> Constraint
--   Show Int :: Constraint
--   Functor  :: (* -> *) -> Constraint  
--   Monad    :: (* -> *) -> Constraint

data Bool'
  = True'
  | False'

-- | What exists at runtime?
-- "Promoted data constructors are of the wrong kind to ever exist at runtime"

data UserType
  = User
  | Admin

{-

data User = User
  { userAdminToken :: Maybe (Proxy 'Admin)
  }

doSensitiveThings :: Proxy 'Admin -> IO ()
doSensitiveThings = undefined

-}

or :: Bool -> Bool -> Bool
or True  _ = True
or False y = y

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True  _ = 'True
  Or 'False y = y

type family Not (x :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

