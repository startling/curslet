module UI.Curslet.Class where

-- | A class that lets us switch some bits on some structure.
class Style s where
  bold      :: Functor f => (Bool -> f Bool) -> s -> f s
  blink     :: Functor f => (Bool -> f Bool) -> s -> f s
  underline :: Functor f => (Bool -> f Bool) -> s -> f s

-- | The system (ANSI-ish) colors.
data System
  = Black | Red     | Green | Yellow
  | Blue  | Magenta | Cyan  | White
  deriving (Eq, Show, Ord, Enum)

-- | A class that lets us see the current color in a structure.
class Color s where
  fg :: Functor f => (Maybe System -> f (Maybe System)) -> s -> f s
  bg :: Functor f => (Maybe System -> f (Maybe System)) -> s -> f s
