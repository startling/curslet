{-# Language ForeignFunctionInterface #-}
module UI.Curslet.Bindings.NCurses where
-- base:
import Data.Char (ord)
import Foreign (Ptr)
import Foreign.C.Types (CInt(..), CChar(..))

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

keypad :: Window -> Int -> IO ()
keypad w n = c_keypad (ptr w) (fromIntegral n) >> return ()

foreign import ccall "ncurses.h newwin"
  c_newwin :: CInt -> CInt -> CInt -> CInt -> IO WindowPtr

newwin :: (Integral a) => (a, a) -> (a, a) -> IO Window
newwin (h, w) (x, y) = fmap Window $ c_newwin (fromIntegral h)
  (fromIntegral w) (fromIntegral x) (fromIntegral y)

foreign import ccall "ncurses.h delwin"
  c_delwin :: WindowPtr -> IO ()

delwin :: Window -> IO ()
delwin = c_delwin . ptr

foreign import ccall "ncurses.h wgetch"
  c_wgetch :: WindowPtr -> IO CInt

-- TODO: higher-level key interface
wgetch :: Window -> IO CInt
wgetch = c_wgetch . ptr

foreign import ccall "ncurses.h wnoutrefresh"
  c_wnoutrefresh :: WindowPtr -> IO CInt

wnoutrefresh = c_wnoutrefresh . ptr

foreign import ccall "ncurses.h doupdate"
  c_doupdate :: IO CInt

foreign import ccall "ncurses.h waddch"
  c_waddch :: WindowPtr -> CChar -> IO CInt

waddch :: Window -> Char -> IO ()
waddch w c = c_waddch (ptr w) (fromIntegral . ord $ c) >> return ()

