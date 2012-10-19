{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
module UI.Curslet.Class where
import Control.Monad
import Control.Applicative 

-- | Some attributes that every Curslet should be able to implement.
data Attribute = Bold | Blink | Reverse | Underline
  deriving (Eq, Ord, Show)
-- TODO: colors

class (Applicative m, Monad m) => Curslet m w | m -> w where
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
  attrs    :: [Attribute] -> m a -> m a

put :: Curslet m w => [Char] -> m ()
put = mapM_ addch

spawn :: Curslet m w
  => (Integer, Integer)
  -> (Integer, Integer) 
  -> m b 
  -> m b
spawn s p a = do
  w <- window s p
  a <* delete w

-- | Go to the beginning of the next line.
newline :: Curslet m w => m ()
newline = position >>= move . flip (,) 0 . (+) 1 . fst

-- TODO: borders?
-- TODO: key interface for getch
-- TODO: colors