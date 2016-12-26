{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Src.Employee (Employee(Employee), firstName, lastName, birthdate,
salary, email, leader, increaseSalary) where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import GHC.Generics
import Src.Unsecure

data Employee = Employee  { firstName  :: String
                          , lastName   :: String
                          , birthdate  :: String
                          , salary     :: Int
                          , email      :: String
                          , leader     :: Bool
                          }
                deriving (Generic)

instance FromJSON Employee
instance ToJSON Employee
instance Show Employee where
  show e =  "\n\n" ++
            "Firstname: " ++ (firstName e) ++
            "\nLastname: " ++ (lastName e) ++
            "\nBirthdate: " ++ (birthdate e) ++
            "\nSalary: " ++ show (salary e) ++
            "\nEmail address: " ++ (email e) ++
            "\nIs a leader?: " ++ show (leader e) ++
            "\n\n"


increaseSalary :: Int -> Employee -> Employee
increaseSalary m e =  Employee f l b s em ml
                      where f  = firstName e
                            l  = lastName e
                            b  = birthdate e
                            s  = salary e + m
                            em = email e
                            ml = leader e
