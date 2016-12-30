{-# LANGUAGE TypeFamilies #-}

module Security.Lattice (Less, Ticket(Ticket)) where

import GHC.Exts (Constraint)

-- | A predicate on types, to be implemented by the provider of a security level type.
-- Actually, means "less or equal".
type family Less sl sh :: Constraint

-- | Proof, that one actually owns a value of type s.
data Ticket s = Ticket
