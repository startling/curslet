{-# Language ForeignFunctionInterface #-}
module UI.Curslet.Bindings.NCurses where
-- base:
import Data.Char (ord, chr)
import Foreign (Ptr, alloca, peek, poke)
import Foreign.C.Types (CInt(..), CChar(..), CWchar)
-- curslet:
import UI.Curslet.Bindings.NCurses.Types

data Window_t
type WindowPtr = Ptr Window_t
newtype Window = Window { ptr :: WindowPtr }

foreign import ccall "ncurses.h initscr"
  c_initscr :: IO WindowPtr

initscr :: IO Window
initscr = fmap Window c_initscr

foreign import ccall "ncurses.h endwin"
  c_endwin :: IO CInt

endwin :: IO ()
endwin = c_endwin >> return ()

foreign import ccall "ncurses.h cbreak"
  c_cbreak :: IO CInt

foreign import ccall "ncurses.h nocbreak"
  c_nocbreak :: IO CInt

foreign import ccall "ncurses.h echo"
  c_echo :: IO CInt

foreign import ccall "ncurses.h noecho"
  c_noecho :: IO CInt

foreign import ccall "ncurses.h raw"
  c_raw :: IO CInt

foreign import ccall "ncurses.h noraw"
  c_noraw :: IO CInt

foreign import ccall "ncurses.h halfdelay"
  c_halfdelay :: CInt -> IO CInt

foreign import ccall "ncurses.h nodelay"
  c_nodelay :: WindowPtr -> CInt -> IO CInt

foreign import ccall "ncurses.h keypad"
  c_keypad :: WindowPtr -> CInt -> IO CInt

keypad :: Window -> IO ()
keypad w = c_keypad (ptr w) 1 >> return ()

nokeypad :: Window -> IO ()
nokeypad w = c_keypad (ptr w) 0 >> return ()

foreign import ccall "ncurses.h newwin"
  c_newwin :: CInt -> CInt -> CInt -> CInt -> IO WindowPtr

newwin :: (Integral a, Integral b) => (a, a) -> (b, b) -> IO Window
newwin (h, w) (x, y) = fmap Window $ c_newwin (fromIntegral h)
  (fromIntegral w) (fromIntegral x) (fromIntegral y)

foreign import ccall "ncurses.h delwin"
  c_delwin :: WindowPtr -> IO ()

delwin :: Window -> IO ()
delwin = c_delwin . ptr

foreign import ccall "ncurses.h wmove"
  c_wmove :: WindowPtr -> CInt -> CInt -> IO CInt

wmove :: (Integral a) => Window -> (a, a) -> IO CInt
wmove w (r, c) = c_wmove (ptr w) (fromIntegral r) (fromIntegral c)

foreign import ccall "ncurses.h wnoutrefresh"
  c_wnoutrefresh :: WindowPtr -> IO CInt

wnoutrefresh = c_wnoutrefresh . ptr

foreign import ccall "ncurses.h doupdate"
  c_doupdate :: IO CInt

foreign import ccall "ncurses.h wadd_wch"
  c_wadd_wch :: WindowPtr -> Ptr Cchar_t -> IO CInt

fromChar :: Char -> Cchar_t
fromChar c = Cchar_t (AttrT 0) [fromIntegral . ord $ c]

wadd_wch :: Char -> WindowPtr -> IO CInt
wadd_wch c w = alloca $ \p -> poke p (fromChar c) >> c_wadd_wch w p

-- TODO: higher-level key interface
foreign import ccall "ncurses.h wget_wch"
  c_wget_wch :: WindowPtr -> Ptr Cchar_t -> IO CInt

wget_wch :: WindowPtr -> IO (Int, Cchar_t)
wget_wch w = alloca $ \p -> do 
  i <- c_wget_wch w p
  c <- peek p
  return (fromIntegral i, c) 

-- TODO: getyx
-- TODO: getmaxyx
