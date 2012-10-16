{-# Language ForeignFunctionInterface #-}
module UI.Curslet.Bindings.NCurses where
-- base:
import Data.Char (ord, chr)
import Data.Function (on)
import Foreign (Ptr, alloca, peek, poke)
import Foreign.C.Types (CInt(..), CChar(..), CWchar)
import Foreign.C.String (castCharToCChar)
-- curslet:
import UI.Curslet.Bindings.NCurses.Types

-- | Windows just wrap a WindowPtr.
newtype Window = Window
  { ptr :: WindowPtr }
  deriving (Eq, Ord)

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

foreign import ccall "ncurses.h box"
  c_box :: WindowPtr -> CChar -> CChar -> IO CInt

box :: Window -> Char -> Char -> IO CInt
box (Window w) = c_box w `on` castCharToCChar

foreign import ccall "ncurses.h wnoutrefresh"
  c_wnoutrefresh :: WindowPtr -> IO CInt

wnoutrefresh :: Window -> IO CInt
wnoutrefresh = c_wnoutrefresh . ptr

foreign import ccall "ncurses.h doupdate"
  c_doupdate :: IO CInt

foreign import ccall "ncurses.h wadd_wch"
  c_wadd_wch :: WindowPtr -> Ptr Cchar_t -> IO CInt

fromChar :: AttrT -> Char -> Cchar_t
fromChar a c = Cchar_t a [fromIntegral . ord $ c]

wadd_wch :: AttrT -> Char -> WindowPtr -> IO CInt
wadd_wch a c w = alloca $ \p -> poke p (fromChar a c)
  >> c_wadd_wch w p

-- TODO: higher-level key interface
foreign import ccall "ncurses.h wget_wch"
  c_wget_wch :: WindowPtr -> Ptr CInt -> IO CInt

wget_wch :: Num a => Window -> IO (Either a Char)
wget_wch (Window w) = alloca $ \p -> do 
  i <- c_wget_wch w p
  c <- peek p
  return $ case i of
    -- OK
    0 -> Right . chr . fromIntegral $ c
    -- KEY_CODE_YES
    400 -> Left . fromIntegral $ c
    -- ERR (-1)

getyx :: (Num t) => Window -> IO (t, t)
getyx (Window w) = do
  y <- c_cur_y w
  x <- c_cur_x w
  return (fromIntegral y, fromIntegral x)

getmaxyx :: (Num t) => Window -> IO (t, t)
getmaxyx (Window w) = do
  y <- c_max_y w
  x <- c_max_x w
  return (fromIntegral y, fromIntegral x)

getbegyx :: (Num t) => Window -> IO (t, t)
getbegyx (Window w) = do
  y <- c_beg_y w
  x <- c_beg_x w
  return (fromIntegral y, fromIntegral x)
