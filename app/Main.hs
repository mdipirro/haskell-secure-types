import Src.Employee
import Src.Unsecure
import Src.JSONHandler

import Data.List (partition)

main :: IO ()
main = do employees <- getEmployees
          let employees' = transform employees (\es -> map (increaseSalary 200) es)
          let e = validate employees
          case e of Left emp    -> print "OK"
                    Right errs  -> print errs
