module Validation.Employee (validate, EmployeeError(Birthdate, Name, Email)) where

import Text.Regex.Posix
import Text.Email.Validate (isValid)
import Data.ByteString.Char8 (pack)
import Src.Employee
--import Src.Unsecure

data EmployeeError =  Birthdate String | Name String | Email String
                      deriving Show

nameRegex   = "^[a-zA-Z =]*$"
dateRegex   = "(^(((0[1-9]|1[0-9]|2[0-8])[/](0[1-9]|1[012]))|((29|30|31)[/](0[13578]|1[02]))|((29|30)[/](0[4,6,9]|11)))[/](19|[2-9][0-9])[0-9][0-9]$)|(^29[/]02[/](19|[2-9][0-9])(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96)$)"

validateDate :: [Employee] -> Maybe EmployeeError
validateDate []     = Nothing
validateDate (e:es) = if b =~ dateRegex
                      then validateDate es
                      else Just (Birthdate ("Incorrect date " ++ b))
                      where b = birthdate e

validateName :: [Employee] -> Maybe EmployeeError
validateName []     = Nothing
validateName (e:es) = if rf && rl
                      then validateName es
                      else Just (Name ("Incorrect name " ++ f ++ " " ++ l))
                      where f   = firstName e
                            l   = lastName e
                            rf  = f =~ nameRegex
                            rl  = l =~ nameRegex

validateEmail :: [Employee] -> Maybe EmployeeError
validateEmail []      = Nothing
validateEmail (e:es)  = if isValid (pack email')
                        then validateEmail es
                        else Just (Email ("Invalid email " ++ email'))
                        where email' = email e

validate :: [[Employee] -> Maybe EmployeeError]
validate = [validateDate, validateName, validateEmail]
