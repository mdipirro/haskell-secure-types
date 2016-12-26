import Src.Model.Employee
import Src.IO.JSONHandler
import Security.Unsecure
import qualified Security.Taint as T

import Data.List (partition)

main :: IO ()
main = do employees <- getEmployees
          let employees'  = transform employees (\es -> map (increaseSalary 200) es)
              e           = validate employees
              t           = pure 10 :: T.Taint Int
          print $ T.value t 0
