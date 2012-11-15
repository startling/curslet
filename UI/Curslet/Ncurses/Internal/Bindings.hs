{-# Language ForeignFunctionInterface #-}
module UI.Curslet.Ncurses.Internal.Bindings where
-- base:
import Foreign
import Foreign.C.Types

foreign import ccall "ncurses.h COLOR_PAIR"
  c_color_pair :: CInt -> IO CInt
