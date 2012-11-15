-- | A dumb little executable that reports on some stuff about
-- your terminal.
module Main where
-- base:
import Foreign.C.Types
-- mtl:
import Control.Monad.Writer
-- curslet:
import UI.Curslet.Ncurses.Internal.Bindings

main :: IO ()
main = runWriterT report >>= return . snd >>= mapM_ see
  where see (k, v) = putStrLn $ k ++ ": " ++ v

-- Tell us all about this terminal.
report :: WriterT [(String, String)] IO ()
report = do
  w <- lift c_initscr
  lift c_start_color
  h <- lift c_has_colors
  tell [("has_colors", show h)]
  c <- lift c_colors
  tell [("COLORS", show c)]
  p <- lift c_color_pairs
  tell [("COLOR_PAIRS", show p)]
  -- Report on the first 16 colors.
  flip mapM_ [0..16] see_color
  lift c_endwin
  return ()

-- Report on a single color pair.
see_color :: CShort -> WriterT [(String, String)] IO ()
see_color c = do
  (Just (fg, bg)) <- lift $ c_pair_content $ fromIntegral c
  (Just f) <- lift $ c_color_content fg
  (Just b) <- lift $ c_color_content bg
  tell [("color #" ++ show c, show f ++ show b)]
  return ()