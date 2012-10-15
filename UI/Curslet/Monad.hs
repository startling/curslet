{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language TemplateHaskell #-}
module UI.Curslet.Monad where
-- base:
import Prelude hiding (mapM_)
import Data.Foldable (mapM_)
import Control.Monad (ap)
import Control.Applicative ((<$>), Applicative(..))
-- mtl
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..), modify)
-- containers:
import Data.Set (Set)
import qualified Data.Set as S
-- lens:
import Control.Lens
-- curslet:
import UI.Curslet.Bindings.NCurses

-- | The immutable state each 'Curslet' can query.
data Internals = Internals
  { _screen :: Window }
makeLenses ''Internals

-- TODO: should this be written instead?
-- | The mutable state each 'Curslet' can query and modify.
data Mutable = Mutable
  { _changed :: Set Window }
makeLenses ''Mutable

-- | A little reader-ish wrapper around IO.
newtype Curslet m = Curslet 
  { io :: Internals -> Mutable -> IO (Mutable, m) }

instance Monad Curslet where
  return a = Curslet $ \i m -> return (m, a)
  (Curslet a) >>= fn = Curslet $ \i m ->
    a i m >>= \(n, x) -> (io . fn $ x) i n

instance Functor Curslet where
  fmap fn (Curslet a) = Curslet $ \i m ->
    fmap (fmap fn) (a i m)

instance Applicative Curslet where
  pure = return
  (<*>) = ap

instance MonadReader Internals Curslet where
  ask = Curslet $ \i m -> return (m, i)
  local fn (Curslet a) = Curslet $ \x -> a (fn x)

instance MonadState Mutable Curslet where
  get = Curslet $ \i m -> return (m, m)
  put s = Curslet $ \i m -> return (s, ())

-- | Make some Window -> IO m function into a Curslet m.
curslet :: (Window -> IO m) -> Curslet m
curslet f = Curslet $ \i m -> (,) m <$> (f . view screen $ i)

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
  (_, r) <- io c (Internals s) (Mutable S.empty)
  -- End the windows, turn on echo and noraw.
  c_echo >> c_noraw >> c_endwin
  return r

-- | Mark this window as modified for the next refresh.
change :: Curslet ()
change = query screen >>= (%=) changed . S.insert

-- | Update all the modified windows.
update :: Curslet ()
update = use changed >>= mapM_ (flip inside . curslet_ $ wnoutrefresh)
  >> changed .= S.empty

-- | Update all the modified windows and redraw them.
refresh :: Curslet ()
refresh = update >> curslet_ (const c_doupdate)

-- | Get a new window.
window
  :: (Integer, Integer) -- ^ the height and width of the new window.
  -> (Integer, Integer) -- ^ y and x position of the new window.
  -> Curslet Window
window a b = Curslet $ \i m -> do
  w <- newwin a b
  keypad w
  return (m, w)

-- | Run some Curslet inside some other window.
inside :: Window -> Curslet a -> Curslet a
inside w = local (screen .~ w)

-- | Delete a window.
delete :: Window -> Curslet ()
delete w = changed %= S.delete w >> curslet (const . delwin $ w)

-- | Current position of the cursor.
position :: Curslet (Integer, Integer)
position = curslet getyx

-- | Get the height and width of the current window.
size :: Curslet (Integer, Integer)
size = over both (+ 1) <$> curslet getmaxyx

-- TODO: does need to mark the window as modified?
-- | Move the cursor.
move :: (Integer, Integer) -> Curslet ()
move c = change >> curslet_ (flip wmove c)

-- | Get a character.
-- TODO: higher-level key interface.
getch :: Curslet (Maybe Char)
getch = either (const Nothing) (Just) <$> curslet wget_wch

-- | Put a character at the cursor position.
addch :: Char -> Curslet ()
addch c = change >> curslet_ (wadd_wch c . ptr)

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
