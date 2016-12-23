module Validation.Employee (validate) where

import Text.Regex.Posix
import Text.Email.Validate (isValid)
import Data.ByteString.Char8 (pack)
import Src.Employee

nameRegex   = "^[a-zA-Z =]*$"
dateRegex   = "(^(((0[1-9]|1[0-9]|2[0-8])[/](0[1-9]|1[012]))|((29|30|31)[/](0[13578]|1[02]))|((29|30)[/](0[4,6,9]|11)))[/](19|[2-9][0-9])[0-9][0-9]$)|(^29[/]02[/](19|[2-9][0-9])(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96)$)"

validateDate :: [Employee] -> Bool
validateDate []     = True
validateDate (e:es) = rd && validateDate es
                      where rd = birthdate e =~ dateRegex

validateName :: [Employee] -> Bool
validateName []     = True
validateName (e:es) = rf && rl && validateName es
                      where rf = firstName e =~ nameRegex
                            rl = lastName e =~ nameRegex

validateEmail :: [Employee] -> Bool
validateEmail []      = True
validateEmail (e:es)  = isValid (pack (email e)) && validateEmail es

validate :: [[Employee] -> Bool]
validate = [validateName, validateDate, validateEmail]
