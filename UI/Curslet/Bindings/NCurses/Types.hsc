{-# Language GeneralizedNewtypeDeriving #-}
#define NCURSES_OPAQUE (0)
#define _XOPEN_SOURCE_EXTENDED
-- alignment operation, stolen from:
-- http://www.haskell.org/haskellwiki/FFICookBook#Working_with_structs
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#include <ncurses.h>
module UI.Curslet.Bindings.NCurses.Types where
-- base:
import Foreign
import Foreign.C.Types
import Data.Int (Int16)
import Control.Applicative ((<$>), (<*>))

-- | A wrapper type for ncurses' attr_t. Probably a numeric type;
-- for me, it's an int32.
newtype AttrT = AttrT 
  { unAttrT :: #{type attr_t} }
  deriving (Eq, Show, Storable)

-- | Wraps ncurses' cchar_t.
data Cchar_t = Cchar_t
  { attr :: AttrT
  , chars :: [CWchar] }
  deriving (Eq, Show)

instance Storable Cchar_t where
  sizeOf = const #{size cchar_t}
  alignment _ = #{alignment cchar_t}
  peek p = Cchar_t
    -- Get an AttrT.
    <$> (AttrT <$> #{peek cchar_t, attr} p)
    -- Get a list of #{const CCHARW_MAX} CWchars.
    <*> peekArray #{const CCHARW_MAX} (#{ptr cchar_t, chars} p)
  poke p c = do
    -- poke the attr_t in.
    #{poke cchar_t, attr} p $ unAttrT . attr $ c
    -- poke the CWchars in, given that there are no more than
    --  #{const CCHARW_MAX} of them.
    if length (chars c) > #{const CCHARW_MAX}
      then fail "Cchar_t contains too many CWchars."
      else pokeArray (#{ptr cchar_t, chars} p) $ chars c
    return ()

data Window_t
type WindowPtr = Ptr Window_t
newtype Window = Window { ptr :: WindowPtr }

c_cur_y :: WindowPtr -> IO #{type NCURSES_SIZE_T}
c_cur_y = #{peek struct _win_st, _cury}

c_cur_x :: WindowPtr -> IO #{type NCURSES_SIZE_T}
c_cur_x = #{peek struct _win_st, _curx}

c_max_y :: WindowPtr -> IO #{type NCURSES_SIZE_T}
c_max_y =  #{peek struct _win_st, _maxy}

c_max_x :: WindowPtr -> IO #{type NCURSES_SIZE_T}
c_max_x =  #{peek struct _win_st, _maxx}

c_beg_y :: WindowPtr -> IO #{type NCURSES_SIZE_T}
c_beg_y =  #{peek struct _win_st, _begy}

c_beg_x :: WindowPtr -> IO #{type NCURSES_SIZE_T}
c_beg_x =  #{peek struct _win_st, _begx}
