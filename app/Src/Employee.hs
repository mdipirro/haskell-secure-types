{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Src.Employee (Employee(Employee)) where

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
increaseSalary (Employee f l b s e ml) m = Employee f l b (s+m) e ml
