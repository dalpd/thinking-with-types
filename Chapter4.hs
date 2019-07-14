{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

module Chapter4 where

import Data.Typeable

-- data Proxy a = Proxy

broken :: forall a b. (a -> b) -> a -> b
broken f a = apply
  where
    apply :: b
    apply = f a

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @ a

type family AlwaysUnit a where
  AlwaysUnit a = ()


-- |  Are all of the following type signatures non-ambiguous?
-- 1. AlwaysUnit a -> a                -- non-
-- 2. b -> AlwaysUnit a -> b           -- non-
-- 3. Show a => AlwaysUnit a -> String -- ambiguous
