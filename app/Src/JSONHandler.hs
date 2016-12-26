module Src.JSONHandler (getEmployees, saveEmployees) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL
import Src.Unsecure
import Src.Employee
import qualified Validation.Employee as EmployeeV

employeesFile :: FilePath
employeesFile = "data/employees.json"

readEmployeesFile :: IO BSL.ByteString
readEmployeesFile = BSL.readFile employeesFile

saveEmployees es = BSL.writeFile employeesFile (encodePretty es)

getEmployees :: IO (Unsecure [Employee] EmployeeV.EmployeeError)
getEmployees =  do  json <- eitherDecode <$> readEmployeesFile
                    case json of
                      Left err -> return (makeUnsecure [] [])
                      Right es -> return (makeUnsecure es (EmployeeV.validate))
