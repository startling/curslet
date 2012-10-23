{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
module UI.Curslet.Class where
-- base:
import Prelude hiding (reverse)
import Control.Monad
import Control.Applicative 

class (Applicative m, Monad m) => Curslet m w s | m -> w s where
  -- | Redraw all the windows that have been changed after
  -- executing some action.
  refresh  :: m a -> m a
  -- | Mark the screen to be cleared at the next refresh
  -- before some action.
  clear    :: m a -> m a
  -- | Create a window, given its height and width and 
  -- y and x positions.
  window   :: (Integer, Integer) -> (Integer, Integer) -> m w
  -- | Execute some action inside some window.
  inside   :: w -> m a -> m a
  -- | Delete some window.
  delete   :: w -> m ()
  -- | Get the current y/x position of the cursor.
  position :: m (Integer, Integer)
  -- | Get the maximum y/x position of the cursor.
  getmax   :: m (Integer, Integer)
  -- | Move the cursor.
  move     :: (Integer, Integer) -> m ()
  -- | Get a character or keypress.
  getch    :: m (Maybe Char)
  -- | Print a character.
  addch    :: Char -> m ()
  -- | Do some action with some attributes switched on.
  attrs    :: [s] -> m a -> m a

-- | Add a string to the screen.
put :: Curslet m w a => [Char] -> m ()
put = mapM_ addch

-- | Run some action inside a child window; delete it afterwards.
spawn :: Curslet m w a
  => (Integer, Integer)
  -> (Integer, Integer) 
  -> m b
  -> m b
spawn s p a = do
  w <- window s p
  a <* delete w

-- | Go to the beginning of the next line.
newline :: Curslet m w a => m ()
newline = position >>= move . flip (,) 0 . (+) 1 . fst

-- | Clear the screen, execute some action, and refresh.
frame :: Curslet m w s => m a -> m a
frame = refresh . clear

-- | A class for styling characters in a 'Curslet'. 
class Styled s where
  bold :: s
  blink :: s
  reverse :: s
  underline :: s

-- | A class for things that are colored by some type of thing.
class Colored c a | a -> c where
  fg :: c -> a
  bg :: c -> a

-- | A class for coloring characters in a 'Curslet' with the
-- eight ANSI colors.
class EightColors c where
  black :: c
  red :: c
  green :: c
  yellow :: c
  blue :: c
  magenta ::w c
  cyan :: c
  white :: c

-- | Synonym for 'reverse' so we don't need to hide the 'reverse'
-- from Prelude everywhere.
reverse_ :: Styled s => s
reverse_ = reverse

-- TODO: borders?
-- TODO: key interface for getch
-- TODO: colors