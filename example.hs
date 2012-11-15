import UI.Curslet (Curslet(..), Attribute(..), put)
import UI.Curslet.Ncurses (runNcurses, Ncurses(..))

main = runNcurses $ do
  refresh $ do
    attrs [Bold] $ put "hello, world"
    attrs [Blink] $ addch '!'
  getch
