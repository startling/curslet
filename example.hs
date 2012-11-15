import UI.Curslet
import UI.Curslet.Ncurses

main = monochrome $ do
  refresh $ do
    attrs [bold] $ put "hello, world"
    attrs [blink] $ addch '!'
  getch
