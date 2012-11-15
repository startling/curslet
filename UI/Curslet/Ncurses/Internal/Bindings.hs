{-# Language ForeignFunctionInterface #-}
{-# Language EmptyDataDecls #-}
module UI.Curslet.Ncurses.Internal.Bindings where
-- base:
import Foreign
import Foreign.C.Types
import Control.Applicative

-- | A dummy type for our pointers to point to.
data Win_t

-- | A Window just wraps a pointer.
newtype Window = Window
  { ptr :: Ptr Win_t }
  deriving (Eq)

foreign import ccall "ncurses.h initscr"
  c_initscr :: IO (Ptr Win_t)

foreign import ccall "ncurses.h has_colors"
  c_has_colors :: IO CInt

foreign import ccall "ncurses.h start_color"
  c_start_color :: IO CInt

foreign import ccall "ncurses.h endwin"
  c_endwin :: IO CInt

foreign import ccall "ncurses.h COLOR_PAIR"
  c_color_pair :: CInt -> IO CInt

foreign import ccall "ncurses.h wget_wch"
  c_wget_wch_ :: Ptr Win_t -> Ptr CInt -> IO CInt

-- | A thin wrapper around 'c_wget_wch_'; gives you a pair, the first
-- first of which is  a return code and the second of which is a char.
c_wget_wch :: Ptr Win_t -> IO (CInt, CInt)
c_wget_wch w = alloca $ \p -> (,) <$> c_wget_wch_ w p <*> peek p
