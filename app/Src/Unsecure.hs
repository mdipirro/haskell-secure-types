module Src.Unsecure (Unsecure, makeUnsecure, extract) where

--type ValidationError a = a
data Unsecure a b = Unsecure ([a -> Maybe b], a)

extract :: Unsecure a b -> Either a [b]
extract (Unsecure (fs, v)) =  if null errors
                              then Left v
                              else Right errors
                              where maybes = map (\f -> f v) fs
                                    justs  = filter (\m -> case m of  Nothing -> False
                                                                      Just _  -> True) maybes
                                    errors = map (\(Just m) -> m) justs

makeUnsecure :: a -> [a -> Maybe b] -> Unsecure a b
makeUnsecure value fs = Unsecure (fs, value)
