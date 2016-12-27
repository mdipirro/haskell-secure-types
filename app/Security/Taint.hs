module Security.Taint (Taint, fmap, pure, (<*>), (>>=), get) where

type State   = Int
newtype ST a  = S (State -> (a, State))
type Taint a  = ST a


get :: ST a -> Maybe a
get s = if st < 5
        then Just v
        else Nothing
        where (v, st) = app s 0

app :: ST a -> State -> (a, State)
app (S st) x =  (a, s + 1)
                where (a, s) = st x

info :: ST a -> State -> a
info st x = (\(a, s) -> a) (app st x)

instance Functor ST where
  fmap g st = S (\s ->  let (x, s') = app st s
                        in (g x, s'))

instance Applicative ST where
  pure x      = S (\s -> (x, s - 1))
  stf <*> stx = S (\s ->  let (f, s')   = app stf s
                              (x, s'')  = app stx s'
                              in (f x, s''))

instance Monad ST where
  return    = pure
  st >>= f  = S (\s ->  let (x, s') = app st s
                        in app (f x) s')
