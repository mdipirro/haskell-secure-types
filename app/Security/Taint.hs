module Security.Taint (Taint, fmap, pure, (<*>), (>>=), app) where

import Data.Configurator
import Data.Configurator.Types (Value)
import System.IO.Unsafe
import Data.Text

type State   = Int
newtype ST a  = S (State -> (a, State))
type Taint a  = ST a

instance Functor ST where
  fmap g st = S (\s ->  let (x, s') = app st s
                        in (g x, s'))

instance Applicative ST where
  pure x      = S (\s -> (x, s + 1))
  stf <*> stx = S (\s ->  let (f, s')   = app stf s
                              (x, s'')  = app stx s'
                              in (f x, s''))

instance Monad ST where
  return    = pure
  st >>= f  = S (\s ->  let (x, s') = app st s
                        in app (f x) s')

app :: ST a -> State -> (a, State)
app (S st) x =  (a, s - 1)
                where (a, s) = st x
