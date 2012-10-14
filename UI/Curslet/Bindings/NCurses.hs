{-# Language ForeignFunctionInterface #-}
module UI.Curslet.Bindings.NCurses where
import Foreign
import Foreign.C.Types

data Window_t
type WindowPtr = Ptr Window_t

foreign import ccall "ncurses.h initscr"
  c_initscr :: IO WindowPtr

foreign import ccall "ncurses.h endwin"
  c_endwin :: IO CInt

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

foreign import ccall "ncurses.h newwin"
  c_newwin :: CInt -> CInt -> CInt -> CInt -> IO WindowPtr

foreign import ccall "ncurses.h delwin"
  c_delwin :: WindowPtr -> IO ()

foreign import ccall "ncurses.h wgetch"
  c_wgetch :: WindowPtr -> IO CInt

foreign import ccall "ncurses.h wnoutrefresh"
  c_wnoutrefresh :: WindowPtr -> IO CInt

foreign import ccall "ncurses.h doupdate"
  c_doupdate :: IO CInt

foreign import ccall "ncurses.h waddch"
  c_waddch :: WindowPtr -> CChar -> IO CInt

