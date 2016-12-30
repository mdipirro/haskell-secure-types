import Src.Model.Employee
import Src.IO.JSONHandler
import Security.Unsecure
import Security.UserTaint
import Security.SecureFlow

main :: IO ()
main = do employees <- getEmployees
          let employees'  = umap employees (\es -> map (increaseSalary 200) es)
              e           = validate employees
              t           = pure (\x y -> x + y) <*> pure 10 <*> pure 20 :: Taint Int
              t'          = pure (\x y z -> x + y + z) <*> pure 30 <*> t <*> pure 10
          print $ getFromConfig t'
          print $ getFromInt t' 10
--print $ open proofLow $ ((declassifyWith (pure (\x -> Nothing) :: Sec H (Int -> Maybe Int)) sec) :: Sec L Int)
