module Src.IO.TestIOFunctions where

import Security.Unsecure

data TestIOError =  NegativeNumber
                deriving Show

getNat :: IO (Unsecure Int TestIOError)
getNat = do n <- getLine
            return $ upure (read n) [(\n ->  if n >= 0
                                            then Nothing
                                            else Just NegativeNumber)]
