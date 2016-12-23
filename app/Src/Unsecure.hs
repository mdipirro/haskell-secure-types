module Src.Unsecure (Unsecure, makeUnsecure, extract) where

newtype Unsecure a = Unsecure ([a -> Bool], a)

extract :: Unsecure a -> Maybe a
extract (Unsecure (fs, v)) =  if and (map (\f -> f v) fs)
                              then Just v
                              else Nothing

makeUnsecure :: a -> [a -> Bool] -> Unsecure a
makeUnsecure value fs = Unsecure (fs, value)
