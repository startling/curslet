{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
module UI.Curslet.Monad where
-- base:
import Control.Monad (ap)
import Control.Applicative ((<$>), Applicative(..))
-- mtl
import Control.Monad.Reader (MonadReader(..))
-- curslet:
import UI.Curslet.Bindings.NCurses

-- | The hidden state inside each Curslet.
data Internals = Internals
  { screen :: Window }

-- | A little reader-ish wrapper around IO.
newtype Curslet m = Curslet 
  { io :: Internals -> IO m }

instance Monad Curslet where
  return = Curslet . const .  return
  (Curslet i) >>= fn = Curslet $ \x -> i x >>= ($ x) . io . fn

instance Functor Curslet where
  fmap fn (Curslet i) = Curslet $ fmap (fmap fn) i

instance Applicative Curslet where
  pure = return
  (<*>) = ap

instance MonadReader Internals Curslet where
  ask = Curslet return
  local fn (Curslet a) = Curslet $ \x -> a (fn x)

-- | Run a Curslet monad in IO.
runCurslet :: Curslet n -> IO n
runCurslet c = do
  -- Turn on raw mode, don't echo.
  c_raw >> c_noecho
  -- Get the screen.
  s <- initscr
  -- Set keypad on it.
  keypad s
  r <- io c (Internals s)
  -- End the windows, turn on echo and raw.
  c_endwin >> c_echo >> c_noraw
  return r

-- | Get a new window.
window :: (Integral a, Integral b)
  => (a, a) -- ^ the height and width of the new window.
  -> (b, b) -- ^ x and y position of the new window.
  -> Curslet Window
window a b = Curslet . const $ do
  w <- newwin a b
  keypad w
  return w

-- | Delete a window.
delete :: Window -> Curslet ()
delete = Curslet . const . delwin
