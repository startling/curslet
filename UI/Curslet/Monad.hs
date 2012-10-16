{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
module UI.Curslet.Monad
  ( Internals
  , screen
  , Curslet
  , runCurslet
  , refresh
  , window
  , inside
  , delete
  , position
  , size
  , move
  , border
  , getch
  , addch
  , put
  , spawn ) where
-- base:
import Control.Monad (ap)
import Control.Applicative ((<$>), Applicative(..))
-- mtl
import Control.Monad.Reader (MonadReader(..))
-- curslet:
import UI.Curslet.Bindings.NCurses

-- | The immutable state each 'Curslet' can query.
data Internals = Internals
  { screen :: Window }

-- | A little reader-ish wrapper around IO.
newtype Curslet m = Curslet 
  { io :: Internals -> IO m }

instance Monad Curslet where
  return = Curslet . const . return
  (Curslet a) >>= fn = Curslet $ \i -> a i >>= ($ i) . io . fn

instance Functor Curslet where
  fmap fn (Curslet a) = Curslet $ fmap (fmap fn) a

instance Applicative Curslet where
  pure = return
  (<*>) = ap

instance MonadReader Internals Curslet where
  ask = Curslet return
  local fn (Curslet a) = Curslet $ \x -> a (fn x)

-- | Make some Window -> IO m function into a Curslet m.
curslet :: (Window -> IO m) -> Curslet m
curslet f = Curslet $ f . screen

-- | Make some Window -> IO m function into a Curslet ()
curslet_ :: (Window -> IO a) -> Curslet ()
curslet_ f = curslet f >> return ()

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

-- | Mark this window as modified for the next refresh.
change :: Curslet ()
change = curslet_ wnoutrefresh

-- | Redraw all the modified windows.
refresh :: Curslet ()
refresh = curslet_ (const c_doupdate)

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
inside w = local $ \x -> x { screen = w }

-- | Delete a window.
delete :: Window -> Curslet ()
delete w = curslet (const . delwin $ w)

-- | Current position of the cursor.
position :: Curslet (Integer, Integer)
position = curslet getyx

-- | Get the height and width of the current window.
size :: Curslet (Integer, Integer)
size = curslet getmaxyx <&> \(a, b) -> (a + 1, b + 1)
  where (<&>) = flip fmap

-- TODO: does need to mark the window as modified?
-- | Move the cursor.
move :: (Integer, Integer) -> Curslet ()
move c = change >> curslet_ (flip wmove c)

-- | Set the border.
border :: Char -> Char -> Curslet ()
border c d = curslet_ (\w -> box w c d)

-- | Get a character. Note that this implicitly refreshes.
-- TODO: higher-level key interface.
getch :: Curslet (Maybe Char)
getch = either (const Nothing) (Just) <$> curslet wget_wch

-- | Put a character at the cursor position.
addch :: Char -> Curslet ()
addch c = curslet_ (wadd_wch c . ptr) >> change

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
