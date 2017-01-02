module Src.Model.SecureEmployee(SEmployee(..), viewPublicDetails, fromEmployee) where

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
viewPublicDetails Ticket se = "Firstname: " ++ (dejust $ firstName se) ++
                              "\nLastname: " ++ (dejust $ lastName se) ++
                              "\nBirthdate: " ++ (dejust $ birthdate se) ++
                              "\nEmail address: " ++ (dejust $ email se) ++
                              "\nIs a leader?: " ++ show (dejust $ leader se)
                              where dejust e = (\(Just a) -> a) $ open medium e
