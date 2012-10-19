{-# Language MultiParamTypeClasses #-}
module UI.Curslet.Ncurses
  ( Ncurses
  , runNcurses ) where
-- base:
import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap)
-- mtl:
import Control.Monad.Reader (MonadReader(..), asks)
-- curslet:
import UI.Curslet.Class
import UI.Curslet.Ncurses.Bindings
import UI.Curslet.Ncurses.Types

-- | The immutable state each 'Ncurses' can query.
data Internals = Internals
  { screen :: Window
  , attribute :: AttrT }

-- | A little reader-ish wrapper around IO.
newtype Ncurses m = Ncurses
  { io :: Internals -> IO m }

instance Monad Ncurses where
  return = Ncurses . const . return
  a >>= fn = Ncurses $ \i -> io a i >>= ($ i) . io . fn

instance Functor Ncurses where
  fmap fn a = Ncurses . fmap (fmap fn) . io $ a 

instance Applicative Ncurses where
  pure = return
  (<*>) = ap

instance MonadReader Internals Ncurses where
  ask = Ncurses return
  local fn a = Ncurses $ \i -> io a (fn i)

-- Mark a window as having been changed.
change :: Ncurses ()
change = Ncurses (wnoutrefresh . screen) >> return ()

instance Curslet Ncurses Window where
  refresh = flip (<*) . Ncurses . const $ c_doupdate
  clear = (>>) $ Ncurses (c_werase . ptr . screen)
  window a b = Ncurses . const $
    newwin a b >>= \w -> keypad w >> return w
  inside w = local $ \x -> x { screen = w }
  delete w = Ncurses . const . delwin $ w
  position = Ncurses $ getyx . screen
  getmax = Ncurses $ getmaxyx . screen
  -- TODO: mark the window as changed after move?
  move a = Ncurses (flip wmove a . screen) >> return ()
  getch = either (const Nothing) Just
    <$> Ncurses (wget_wch . screen)
  addch c = flip (>>) change . Ncurses
    $ \i -> wadd_wch (attribute i) c (ptr . screen $ i)
  attrs as = local $ \i -> let a = attribute i in
    i { attribute = addAttributes a as }

-- Run an Ncurses in IO.
runNcurses :: Ncurses a -> IO a
runNcurses c = do
  -- Intialize things, get the main screen.
  s <- initscr
  -- Turn on colors, raw mode, and noecho.
  c_start_color >> c_raw >> c_noecho
  -- Set keypad on the main screen.
  keypad s
  -- Run the action.
  r <- io c (Internals s 0)
  -- End the windows, turn on echo and noraw.
  c_echo >> c_noraw >> c_endwin
  -- Return the result of the Ncurses.
  return r
