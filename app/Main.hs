import Src.Employee
import Src.Unsecure
import Src.JSONHandler

--limitSalary :: Int -> [Employee] -> Bool
--limitSalary _ []                      = True
--limitSalary n ((Employee f l b s le):es) = s <= n && limitSalary n es

main :: IO ()
main = do employees <- getEmployees
          let e = extract employees
          print e
