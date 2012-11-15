#define NCURSES_OPAQUE (0)
#define _XOPEN_SOURCE_EXTENDED
#include <ncurses.h>
module UI.Curslet.Ncurses.Internal.Header where
-- base:
import Foreign
import Foreign.C.Types

-- | A type synonym with the haskell type equivalent to the
-- one that ncurses uses for its attributes.
type Attr_t = #{type attr_t}

-- A haskell synonym for A_BOLD.
a_bold :: Attr_t
a_bold = #{const A_BOLD}

-- A haskell synonym for A_BLINK.
a_blink :: Attr_t
a_blink = #{const A_BLINK}

-- A haskell synonym for A_UNDERLINE.
a_underline :: Attr_t
a_underline = #{const A_UNDERLINE}

-- A haskell synonym for COLOR_BLACK.
color_black :: CInt
color_black = #{const COLOR_BLACK}

-- A haskell synonym for COLOR_RED.
color_red :: CInt
color_red = #{const COLOR_RED}

-- A haskell synonym for COLOR_GREEN.
color_green :: CInt
color_green = #{const COLOR_GREEN}

-- A haskell synonym for COLOR_YELLOW.
color_yellow :: CInt
color_yellow = #{const COLOR_YELLOW}

-- A haskell synonym for COLOR_BLUE.
color_blue :: CInt
color_blue = #{const COLOR_BLUE}

-- A haskell synonym for COLOR_MAGENTA.
color_magenta :: CInt
color_magenta = #{const COLOR_MAGENTA}

-- A haskell synonym for COLOR_CYAN.
color_cyan :: CInt
color_cyan = #{const COLOR_CYAN}

-- A haskell synonym for COLOR_WHITE.
color_white :: CInt
color_white = #{const COLOR_WHITE}

-- A haskell synonym for ERR.
c_err :: CInt
c_err = #{const ERR}

-- A haskell synonym for OK.
c_ok :: CInt
c_ok = #{const OK}

-- A haskell synonym for KEY_CODE_YES.
c_key_code_yes :: CInt
c_key_code_yes = #{const KEY_CODE_YES}
