{-# LANGUAGE DeriveGeneric #-}

module Src.Model.Credential (Credential(..), email, password) where

import Data.Aeson
import GHC.Generics

data Credential = Credential  { email     :: String
                              , password  :: String
                              }
                              deriving (Generic)

instance FromJSON Credential
instance ToJSON Credential
instance Show Credential where
  show c =  "\n\n" ++
            "Email address: " ++ (email c) ++
            "\nPassword: " ++ (password c) ++
            "\n\n"
