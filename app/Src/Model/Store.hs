{-# LANGUAGE DeriveGeneric #-}

module Src.Model.Store (Store(..), productName, price, stocks) where

import Data.Aeson
import GHC.Generics

data Store = Store  { productName   :: String
                    , price         :: Double
                    , stocks        :: Int
                    }
                    deriving (Generic)

instance FromJSON Store
instance ToJSON Store
instance Show Store where
  show s =  "\n\n" ++
            "Product Name: " ++ (productName s) ++
            "\nPrice: " ++ (show $ price s) ++
            "\nStocks: " ++ (show $ stocks s) ++
            "\n\n"
