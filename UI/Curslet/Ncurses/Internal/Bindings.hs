{-# Language ForeignFunctionInterface #-}
{-# Language EmptyDataDecls #-}
module UI.Curslet.Ncurses.Internal.Bindings where
-- base:
import Foreign
import Foreign.C.Types
import Control.Applicative
import Data.Char (chr)
-- curslet
import UI.Curslet.Ncurses.Internal.Header

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

foreign import ccall "ncurses.h can_change_color"
  c_can_change_color :: IO CInt

foreign import ccall "ncurses.h color_content"
  c_color_content_ :: CShort -> Ptr CShort -> Ptr CShort
    -> Ptr CShort -> IO CInt

-- | A thin wrapper around 'c_color_content_'; takes a CShort as
-- a color index and gives you the red, green, and blue values
-- as a triplet of CShorts.
c_color_content :: CShort -> IO (Maybe (CShort, CShort, CShort))
c_color_content n = alloca $ \a -> alloca $ \b -> alloca $ \c -> do
  r <- c_color_content_ n a b c
  if r == c_ok
    then fmap Just $ (,,) <$> peek a <*> peek b <*> peek c
    else return Nothing

foreign import ccall "ncurses.h pair_content"
  c_pair_content_ :: CShort -> Ptr CShort -> Ptr CShort -> IO CInt

-- | A thing wrapper around 'c_pair_content'; takes a CShort
-- as a pair index and returns the colors contained therein.
c_pair_content :: CShort -> IO (Maybe (CShort, CShort))
c_pair_content n = alloca $ \a -> alloca $ \b -> do
  r <- c_pair_content_ n a b
  if r == c_ok
    then fmap Just $ (,) <$> peek a <*> peek b
    else return Nothing

foreign import ccall "ncurses.h start_color"
  c_start_color :: IO CInt

foreign import ccall "ncurses.h &COLORS"
  c_colors_ :: Ptr CInt

-- | Thin wrapper around 'c_colors_'; just peeks at the value there.
c_colors :: IO CInt
c_colors = peek c_colors_

foreign import ccall "ncurses.h &COLOR_PAIRS"
  c_color_pairs_ :: Ptr CInt

c_color_pairs :: IO CInt
c_color_pairs = peek c_color_pairs_

foreign import ccall "ncurses.h endwin"
  c_endwin :: IO CInt

foreign import ccall "ncurses.h COLOR_PAIR"
  c_color_pair :: CInt -> IO CInt

foreign import ccall "ncurses.h wget_wch"
  c_wget_wch_ :: Ptr Win_t -> Ptr CInt -> IO CInt

-- | A thin wrapper around 'c_wget_wch_'; gives you either a CInt
-- representing some keypress or a Char.
c_wget_wch :: Ptr Win_t -> IO (Maybe (Either CInt Char))
c_wget_wch w = alloca $ \p -> do
  -- The return code.
  r <- c_wget_wch_ w p
  -- The wide char or key code.
  c <- peek p
  -- If it's key_code_yes, we got a key code.
  return $ if r == c_key_code_yes then Just $ Left c
    -- If it's ok, then we got a character.
    else if r == c_ok then Just . Right . chr . fromIntegral $ c
    else Nothing
