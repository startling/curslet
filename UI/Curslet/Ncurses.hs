module UI.Curslet.Ncurses where
-- base
import Data.Bits ((.|.))
-- curslet
import UI.Curslet.Class
import UI.Curslet.Ncurses.Internal.Header
import UI.Curslet.Ncurses.Internal.Bindings

data Attr = Attr
  { _bold      :: Bool
  , _blink     :: Bool
  , _underline :: Bool
  , _fg        :: Maybe System
  , _bg        :: Maybe System }
  deriving (Eq, Show)

bitfield :: Attr -> Attr_t
bitfield (Attr b l u f g) = foldr (.|.) 0
  [ if b then a_bold else 0
  , if l then a_blink else 0
  , if u then a_underline else 0 ]

instance Style Attr where
  bold fn a = fn (_bold a) <&> \b -> a { _bold = b }
    where (<&>) = flip fmap
  blink fn a = fn (_blink a) <&> \b -> a { _blink = b }
    where (<&>) = flip fmap
  underline fn a = fn (_underline a) <&> \u -> a { _underline = u }
    where (<&>) = flip fmap

instance Color Attr where
  fg fn a = fn (_fg a) <&> \f -> a { _fg = f }
    where (<&>) = flip fmap
  bg fn a = fn (_bg a) <&> \f -> a { _bg = f }
    where (<&>) = flip fmap
