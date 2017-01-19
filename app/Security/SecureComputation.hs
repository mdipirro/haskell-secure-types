{-# LANGUAGE  TypeFamilies #-}

module Security.SecureComputation (SecureComputation(..), open, P, T, spure,
smap, sapp, sbind) where

import GHC.Exts (Constraint)

data SecureComputation m a = SC a

type family MustBePure m :: Constraint

data P = P
data T = T

type instance (MustBePure P) = ()

open :: MustBePure m => SecureComputation m a -> a
open (SC a) = a

smap :: (MustBePure m, MustBePure m') => (a -> b) -> SecureComputation m a
          -> SecureComputation m' b
smap f (SC a) = SC $ f a

spure :: a -> SecureComputation m a
spure = SC

sapp :: (MustBePure m, MustBePure m') => SecureComputation m' (a -> b)
        -> SecureComputation m a -> SecureComputation m' b
sapp (SC f) sc = smap f sc

sreturn :: a -> SecureComputation m a
sreturn = spure

sbind :: (MustBePure m, MustBePure m') => SecureComputation m a
          -> (a -> SecureComputation m' b) -> SecureComputation m' b
sbind (SC a) f = f a
