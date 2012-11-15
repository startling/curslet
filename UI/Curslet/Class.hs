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

-- | Move the cursor up.
up :: Curslet m w a => m ()
up = do
  (y, x) <- position
  move (y - 1, x)

-- | Move the cursor down.
down :: Curslet m w a => m ()
down = do
  (y, x) <- position
  move (y + 1, x)

-- | Move the cursor left.
left :: Curslet m w a => m ()
left = do
  (y, x) <- position
  move (y, x - 1)

-- | Move the cursor right.
right :: Curslet m w a => m ()
right = do
  (y, x) <- position
  move (y, x + 1)

-- | Add a character in place.
inPlace :: Curslet m w a => Char -> m ()
inPlace c = addch c >> left

-- | Center along the y axis.
centerY :: Curslet m w a => m (Integer, Integer)
centerY = do
  (y, _) <- getmax
  (_, x) <- position
  let c = (y `div` 2, x)
  move c >> return c

-- | Center along the x axis.
centerX :: Curslet m w a => m (Integer, Integer)
centerX = do
  (_, x) <- getmax
  (y, _) <- position
  let c = (y, x `div` 2)
  move c >> return c

-- | Center the cursor on the screen
center :: Curslet m w a => m (Integer, Integer)
center = centerX >> centerY >> position

-- | A class for styling characters in a 'Curslet'. 
class Styled s where
  bold :: s
  blink :: s
  reverse :: s
  underline :: s

-- | Synonym for 'reverse' so we don't need to hide the 'reverse'
-- from Prelude everywhere.
reverse_ :: Styled s => s
reverse_ = reverse

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

-- TODO: borders?
-- TODO: key interface for getch
