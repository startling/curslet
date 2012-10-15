{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
module UI.Curslet.Monad where
-- base:
import Control.Monad (ap, mapM_)
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

-- | Make some Window -> IO m function into a Curslet m.
curslet :: (Window -> IO m) -> Curslet ()
curslet f = Curslet (f . screen) >> return ()

-- | Run a Curslet monad in IO.
runCurslet :: Curslet n -> IO n
runCurslet c = do
  -- Get the screen.
  s <- initscr
  -- Turn on raw mode, don't echo.
  c_raw >> c_noecho
  -- Set keypad on it.
  keypad s
  r <- io c (Internals s)
  -- End the windows, turn on echo and noraw.
  c_echo >> c_noraw >> c_endwin
  return r

-- | Get a new window.
window
  :: (Integer, Integer) -- ^ the height and width of the new window.
  -> (Integer, Integer) -- ^ y and x position of the new window.
  -> Curslet Window
window a b = Curslet . const $ do
  w <- newwin a b
  keypad w
  return w

-- | Run some Curslet inside some other window.
inside :: Window -> Curslet a -> Curslet a
inside w = local (\x -> x { screen = w }) 

-- | Delete a window.
delete :: Window -> Curslet ()
delete = curslet . const . delwin

-- | Current position of the cursor.
position :: Curslet (Integer, Integer)
position = Curslet $ getyx . screen

-- | Move the cursor.
move :: (Integer, Integer) -> Curslet ()
move c = curslet $ flip wmove c

-- | Get a character.
-- TODO: high-level-ish key interface.
getch = curslet wget_wch

-- | Put a character at the cursor position.
addch :: Char -> Curslet ()
addch c = curslet $ wadd_wch c . ptr

-- | Put a string at the cursor position.
put :: String -> Curslet ()
put = mapM_ addch

-- | Run some action inside a new window; delete it afterwards.
spawn
  :: (Integer, Integer) -- ^ The height and width of the window.
  -> (Integer, Integer) -- ^ The y and x positions of the window.
  -> Curslet b -> Curslet b
spawn n b a = do
  w <- window n b
  r <- inside w a
  delete w
  return r

-- TODO: getch keys
-- TODO: colors and attributes (stick colors in Internals)

