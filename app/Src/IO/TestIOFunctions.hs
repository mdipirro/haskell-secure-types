module Src.IO.TestIOFunctions where

import Data.Char
import Security.Unsecure
import Security.SecureComputation

data TestIOError =  NegativeNumber | NonNumeric
                deriving Show

getNat :: IO (Unsecure String TestIOError)
getNat = do n <- getLine
            return $ upure n   [
                                  (\s ->  if null $ dropWhile isDigit s
                                          then Nothing
                                          else Just NonNumeric),
                                  (\n ->  if read n >= 0
                                          then Nothing
                                          else Just NegativeNumber)
                                ]

getUnpureNat :: IO (SecureComputation T String)
getUnpureNat = do n <- getLine
                  return $ spure n
