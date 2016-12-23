{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Src.Employee (Employee(Employee), firstName, lastName, birthdate,
salary, email, leader) where

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
                          , leader     :: Maybe Bool
                          }
                deriving (Show, Generic)

instance FromJSON Employee
instance ToJSON Employee

increaseSalary :: Employee -> Int -> Employee
increaseSalary e m =  Employee f l b s em ml
                      where f  = firstName e
                            l  = lastName e
                            b  = birthdate e
                            s  = salary e + m
                            em = email e
                            ml = leader e
