import Src.Employee
import Src.Unsecure
import Src.JSONHandler

main :: IO ()
main = do employees <- getEmployees
          let e = extract employees
          print e
