{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

class HasFoo a where
  foo :: a -> Int

class C a where
  withC :: a -> Int

class D a where
  withD :: a -> Int

data FooStrategy = WithC | WithD

type family FooStrategyFamily a :: FooStrategy

class FooStrategyFamily a ~ strat => HasFoo' a strat where
  foo' :: a -> Int

instance (FooStrategyFamily a ~ 'WithC, C a) => HasFoo' a 'WithC where
  foo' = withC

instance (FooStrategyFamily a ~ 'WithD, D a) => HasFoo' a 'WithD where
  foo' = withD

instance HasFoo' a strat => HasFoo a where
  foo = foo'


instance C [a] where
  withC = length

type instance FooStrategyFamily [a] = 'WithC
-- >>> foo [1,2,3,4]
-- 4
