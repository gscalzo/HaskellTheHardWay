import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

main :: IO ()
main = do 
  (progname, _) <- getArgsAndInitialize
  createWindow "Hello World"
  displayCallback $= flush
  mainLoop