{-# LANGUAGE TypeFamilies #-}

module Security.Lattice (LEQ, Ticket(Ticket)) where

import GHC.Exts (Constraint)

-- Implementation based on Russo et al., 2008

type family LEQ sl sh :: Constraint

data Ticket s = Ticket
