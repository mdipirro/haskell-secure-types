module Src.JSONHandler (getEmployees, saveEmployees) where

import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Text.Lazy.IO as LazyIO
import Data.Aeson.Text (encodeToLazyText)
import Src.Unsecure
import Src.Employee
import qualified Validation.Employee as EmployeeV

employeesFile :: FilePath
employeesFile = "data/employees.json"

readEmployeesFile :: IO ByteString.ByteString
readEmployeesFile = ByteString.readFile employeesFile

saveEmployees es = LazyIO.writeFile employeesFile (encodeToLazyText es)

getEmployees :: IO (Unsecure [Employee])
getEmployees =  do  json <- eitherDecode <$> readEmployeesFile
                    case json of
                      Left err -> return (makeUnsecure [] [])
                      Right es -> return (makeUnsecure es (EmployeeV.validate))
