module Src.Unsecure (Unsecure, makeUnsecure, validate, transform) where

data Unsecure a b = Unsecure ([a -> Maybe b], a)

validate :: Unsecure a b -> Either a [b]
validate (Unsecure (fs, v)) = if null errors
                              then Left v
                              else Right errors
                              where maybes = map (\f -> f v) fs
                                    justs  = filter (\m -> case m of  Nothing -> False
                                                                      Just _  -> True) maybes
                                    errors = map (\(Just m) -> m) justs

transform :: Unsecure a b -> (a -> a) -> Unsecure a b
transform (Unsecure (fs, v)) f = Unsecure (fs, f v)

makeUnsecure :: a -> [a -> Maybe b] -> Unsecure a b
makeUnsecure value fs = Unsecure (fs, value)
