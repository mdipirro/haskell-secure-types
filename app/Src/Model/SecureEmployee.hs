module Src.Model.SecureEmployee(SEmployee, viewPublicDetails, fromEmployee,
firstName, lastName, birthdate, salary, email, leader, increaseSalary) where

import qualified Src.Model.Employee as E
import Security.SecureFlow
import Security.Lattice
import Security.ThreeLevels

data SEmployee = SEmployee  { firstName  :: SecureFlow Low String
                            , lastName   :: SecureFlow Low String
                            , birthdate  :: SecureFlow Low String
                            , salary     :: SecureFlow High Int
                            , email      :: SecureFlow Medium String
                            , leader     :: SecureFlow Low Bool
                            }

fromEmployee :: E.Employee -> SEmployee
fromEmployee e = SEmployee  (pure $ E.firstName e)
                            (pure $ E.lastName e)
                            (pure $ E.birthdate e)
                            (pure $ E.salary e)
                            (pure $ E.email e)
                            (pure $ E.leader e)

viewPublicDetails :: LEQ Medium s => Ticket s -> SEmployee -> String
viewPublicDetails Ticket se = "Firstname: " ++ (open' $ firstName se) ++
                              "\nLastname: " ++ (open' $ lastName se) ++
                              "\nBirthdate: " ++ (open' $ birthdate se) ++
                              "\nEmail address: " ++ (open' $ email se) ++
                              "\nIs a leader?: " ++ show (open' $ leader se)
                              where open' e = open medium e

increaseSalary :: Int -> SEmployee -> SEmployee
increaseSalary i se = SEmployee f l b s e le
                      where f = firstName se
                            l = lastName se
                            b = birthdate se
                            s = fmap (\s -> s+i) $ salary se
                            e = email se
                            le = leader se
