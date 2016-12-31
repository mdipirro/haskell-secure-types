{-# LANGUAGE TypeFamilies #-}

module Security.Lattice (LEQ, Ticket(Ticket)) where

import GHC.Exts (Constraint)

-- Implementation based on Russo et al., 2008

-- | A predicate on types, to be implemented by the provider of a security level type.
-- Actually, means "less or equal".
type family LEQ sl sh :: Constraint

-- | Proof, that one actually owns a value of type s.
data Ticket s = Ticket
