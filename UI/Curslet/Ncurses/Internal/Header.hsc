#define NCURSES_OPAQUE (0)
#define _XOPEN_SOURCE_EXTENDED
#include <ncurses.h>
module UI.Curslet.Ncurses.Internal.Header where
-- base:
import Foreign

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
