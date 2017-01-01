module Security.SecureFlow (SecureFlow, Hatch, open, up, declassifyWith) where

import Control.Monad.Identity
import Control.Applicative

-- Implementation based on Russo et al., 2008

import Security.Lattice

-- | SecureFlow: the Identity monad tagged with a proposition allowing to access
-- its value.
data SecureFlow s a = Allowed a | Denied
type Hatch s a b = SecureFlow s (a -> Maybe b)

instance Functor (SecureFlow s) where
    fmap f (Allowed x)  = Allowed $ f x
    fmap _ Denied       = Denied

instance Applicative (SecureFlow s) where
    pure x                      = Allowed x
    (Allowed f) <*> (Allowed x) = Allowed $ f x
    _ <*> _                     = Denied

instance Monad (SecureFlow s) where
    return            = pure
    (Allowed a) >>= f = f a
    Denied >>= _      = Denied
    fail _            = Denied

-- | API for handling SecureFlow values. Raising the security level of a value with @up@
-- is possible for everybody. A SecureFlow value can be unwrapped given a proof that
-- accessing the security level of it was allowed.

open :: LEQ s s' => Ticket s' -> SecureFlow s a -> Maybe a
open Ticket (Allowed a) = Just a
open Ticket Denied      = Nothing

up :: LEQ s s' => SecureFlow s a -> SecureFlow s' a
up (Allowed a)  = Allowed a
up Denied       = Denied

-- | Declassification
unsafeCoerceLevels :: LEQ s' s => SecureFlow s a -> SecureFlow s' a
unsafeCoerceLevels (Allowed x)  = Allowed x
unsafeCoerceLevels Denied       = Denied

declassifyWith :: (LEQ s k, LEQ s' s) => Hatch k a b -> SecureFlow s a -> SecureFlow s' b
declassifyWith (Allowed f) s = unsafeCoerceLevels $ do  x <- s
                                                        case f x of
                                                          Just x' -> return x'
                                                          Nothing -> fail ""
declassifyWith Denied _      = Denied
