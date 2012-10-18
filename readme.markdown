# curslet.

This is a work-in-progress terminal UI library for haskell. Some
things still need to be done:

* Colors
* Pads
* Borders
* High-level keypress interface

It's built on ncurses at the moment, but I'm considering implemented
another backend or two in the future.

Here's what the api looks like:

````haskell
import UI.Curslet (Curslet(..), Attribute(..))
import UI.Curslet.Ncurses (runNcurses)

main = runNcurses $ do
  refresh $ do
    attrs [Bold] $ print "hello, world"
	attrs [Blink] $ addch '!'
  getch

````

