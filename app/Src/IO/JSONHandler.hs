module Src.IO.JSONHandler (getEmployees, saveEmployees) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL
import Security.Unsecure
import Src.Model.Employee
import qualified Src.Validation.Employee as EmployeeV

employeesFile :: FilePath
employeesFile = "data/employees.json"

readEmployeesFile :: IO BSL.ByteString
readEmployeesFile = BSL.readFile employeesFile

saveEmployees es = BSL.writeFile employeesFile (encodePretty es)

getEmployees :: IO (Unsecure [Employee] EmployeeV.EmployeeError)
getEmployees =  do  json <- eitherDecode <$> readEmployeesFile
                    case json of
                      Left err -> return (upure [] [])
                      Right es -> return (upure es (EmployeeV.validate))
