module UI.Curslet.Ncurses where
-- base
import Data.Bits ((.|.))
-- curslet
import UI.Curslet.Attributes
import UI.Curslet.Ncurses.Internal.Header

data Attr = Attr
  { _bold      :: Bool       
  , _blink     :: Bool
  , _underline :: Bool }
  deriving (Eq, Show)

bitfield :: Attr -> Attr_t
bitfield (Attr b l u) = foldr (.|.) 0
  [ if b then a_bold else 0
  , if l then a_blink else 0
  , if u then a_underline else 0 ]

instance Style Attr where
  bold fn (Attr b l u) = fn b <&> \b -> Attr b l u
    where (<&>) = flip fmap
  blink fn (Attr b l u) = fn l <&> \l -> Attr b l u
    where (<&>) = flip fmap
  underline fn (Attr b l u) = fn u <&> \u -> Attr b l u
    where (<&>) = flip fmap
