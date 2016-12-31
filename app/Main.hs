{-# LANGUAGE TypeFamilies #-}

import Src.Model.Employee
import Src.IO.JSONHandler
import Security.Unsecure
import Security.UserTaint
import Security.SecureFlow
import Security.Lattice
--import Security.MonadicFlow

data L = L
data H = H

type instance (LEQ L L) = ()
type instance (LEQ L H) = ()
type instance (LEQ H H) = ()

proofLow :: Ticket L
proofLow = Ticket

proofHigh :: Ticket H
proofHigh = Ticket

sum10 :: Hatch H Int Int
sum10 = pure (\x -> Just $ x+10)


main :: IO ()
main = do employees <- getEmployees
          let employees'  = umap employees (\es -> map (increaseSalary 200) es)
              e           = validate employees
              t           = pure (\x y -> x + y) <*> pure 10 <*> pure 20 :: Taint Int
              t'          = pure (\x y z -> x + y + z) <*> pure 30 <*> t <*> pure 10
              sec         = pure 10 :: SecureFlow H Int
          print $ getFromConfig t'
          print $ getFromInt t' 10
          print $ open proofLow $ ((declassifyWith sum10 sec) :: SecureFlow L Int)
