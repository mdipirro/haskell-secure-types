module Security.Unsecure (Unsecure, upure, validate, umap) where

type ValidationFunctions a b = [a -> Maybe b]
data Unsecure a b = Unsecure (ValidationFunctions a b, a)

validate :: Unsecure a b -> Either a [b]
validate (Unsecure (fs, v)) = if null errors
                              then Left v
                              else Right errors
                              where maybes = map (\f -> f v) fs
                                    justs  = filter (\m -> case m of  Nothing -> False
                                                                      Just _  -> True) maybes
                                    errors = map (\(Just m) -> m) justs

umap :: Unsecure a b -> (a -> a) -> Unsecure a b
umap (Unsecure (fs, v)) f = Unsecure (fs, f v)

upure :: a -> ValidationFunctions a b -> Unsecure a b
upure value fs = Unsecure (fs, value)
