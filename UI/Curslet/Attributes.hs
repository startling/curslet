module UI.Curslet.Attributes where

-- | A class that lets us switch some bits on some structure.
class Style s where
  bold      :: Functor f => (Bool -> f Bool) -> s -> f s
  blink     :: Functor f => (Bool -> f Bool) -> s -> f s
  underline :: Functor f => (Bool -> f Bool) -> s -> f s
