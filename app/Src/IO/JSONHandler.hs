module Src.IO.JSONHandler (getEmployees, saveEmployees, getCredentials) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as BSL
import Security.Unsecure
import Security.SecureFlow
import Security.ThreeLevels
import Src.Model.Employee
import Src.Model.Credential
import Src.Model.Store
import qualified Src.Validation.Employee as EmployeeV

employeesFile :: FilePath
employeesFile = "data/employees.json"

storesFile :: FilePath
storesFile = "data/stores.json"

credentialsFile :: FilePath
credentialsFile = "data/credentials.json"

--readEmployeesFile :: IO BSL.ByteString
--readEmployeesFile = BSL.readFile employeesFile

saveEmployees es = BSL.writeFile employeesFile (encodePretty es)

getEmployees :: IO (Unsecure [Employee] EmployeeV.EmployeeError)
getEmployees =  do  json <- eitherDecode <$> BSL.readFile employeesFile
                    case json of  Left err -> return (upure [] [])
                                  Right es -> return (upure es (EmployeeV.validate))

getCredentials :: IO (SecureFlow High [Credential])
getCredentials = do json <- eitherDecode <$> BSL.readFile credentialsFile
                    case json of  Left err -> return (fail "")
                                  Right cs -> return (pure cs)
