module Validation.Employee (validate) where

import Text.Regex.Posix
import Src.Employee

nameRegex   = "^[a-zA-Z =]*$"
dateRegex   = "(^(((0[1-9]|1[0-9]|2[0-8])[/](0[1-9]|1[012]))|((29|30|31)[/](0[13578]|1[02]))|((29|30)[/](0[4,6,9]|11)))[/](19|[2-9][0-9])[0-9][0-9]$)|(^29[/]02[/](19|[2-9][0-9])(00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96)$)"
emailRegex  = "^[_A-Za-z0-9-]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$"

validateDate :: [Employee] -> Bool
validateDate []                           = True
validateDate ((Employee f l b s e ml):es) = rd && validateDate es
                                            where rd = b =~ dateRegex

validateName :: [Employee] -> Bool
validateName []                           = True
validateName ((Employee f l b s e ml):es) = rf && rl && validateName es
                                            where rf = f =~ nameRegex
                                                  rl = l =~ nameRegex

validateEmail :: [Employee] -> Bool
validateEmail []                            = True
validateEmail ((Employee f l b s e ml):es)  = re && validateEmail es
                                              where re = e =~ emailRegex

validate :: [[Employee] -> Bool]
validate = [validateName, validateDate, validateEmail]
