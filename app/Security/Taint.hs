module Security.Taint (Taint, fmap, pure, (<*>), (>>=), value) where

type State   = Int
newtype ST a  = S (State -> (a, State))
type Taint a  = ST a

value :: ST a -> State -> a
value s x = fst $ app s x

app :: ST a -> State -> (a, State)
app (S st) x = st x

info :: ST a -> State -> a
info st x = (\(a, s) -> a) (app st x)

instance Functor ST where
  fmap g st = S (\s ->  let (x, s') = app st s
                        in (g x, s'))

instance Applicative ST where
  pure x      = S (\s -> (x, s))
  stf <*> stx = S (\s ->  let (f, s')   = app stf s
                              (x, s'')  = app stx s'
                              in (f x, s''))

instance Monad ST where
  return    = pure
  st >>= f  = S (\s ->  let (x, s') = app st s
                        in app (f x) s')
