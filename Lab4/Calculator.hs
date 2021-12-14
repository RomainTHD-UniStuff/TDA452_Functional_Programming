-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = 
  do -- Create them user interface elements
  canvas  <- mkCanvas canWidth canHeight   -- The drawing area
  zoomL   <- mkHTML "Zoom +/-"             -- The text "Zoom +/-"
  slider  <- mkSlider (100, 300) 100       -- The zoom slider
  fx      <- mkHTML "<i>f</i>(<i>x</i>)= " -- The text "f(x)="
  input   <- mkInput 20 ""                 -- The formula input
  reset   <- mkButton "Reset"              -- The reset button
  draw    <- mkButton "Draw graph"         -- The draw button
  diff    <- mkButton "Differentiate"      -- The differentiate button

  -- Add the user interface elements to the page, creating a specific layout
  zoomBar <- row [pure zoomL, pure slider]
  formula <- row [pure fx, pure input]
  buttons <- row [pure reset, pure draw, pure diff]
  getBody window #+ [column [pure canvas, pure zoomBar, pure formula, pure buttons]]

  -- Styling
  getBody window # set style [("backgroundColor","lightblue"),
                              ("textAlign","center")]
  pure input # set style [("fontSize","14pt")]
  
  -- Interaction (install event handlers)
  on UI.click     reset $ \ _ -> do
    resetCanvas canvas slider
    resetSlider slider
    disableSlider slider
    pure input # set value ""
  on UI.click     draw  $ \ _ -> readAndDraw input canvas slider
  on valueChange' input $ \ _ -> readAndDraw input canvas slider
  on UI.click     diff  $ \ _ -> do
    formula <- get value input
    case readExpr formula of
      Just exp -> pure input # set value (showExpr (differentiate exp))
      Nothing  -> pure input
    readAndDraw input canvas slider
  on valueChange' slider $ \ _ -> zoom input canvas slider

  -- Init canvas
  disableSlider slider
  resetCanvas canvas slider

resetCanvas :: Canvas -> Element -> UI()
resetCanvas canvas slider = do
  clearCanvas canvas
  path "black" [(0, canHeight/2), (canWidth, canHeight/2)] canvas
  path "black" [(canWidth/2, 0), (canWidth/2, canHeight)] canvas
  UI.fillText "0" (canWidth/2 + 5, canHeight/2 + 12) canvas

disableSlider :: Element -> UI Element
disableSlider slider = do pure slider # set style [("cursor","not-allowed"), ("pointer-events", "none")]

resetSlider :: Element -> UI Element
resetSlider slider = pure slider # set value (show 1)

readAndDraw :: Element -> Canvas -> Element -> UI ()
readAndDraw input canvas slider = do
  resetSlider slider
  printCanvas 0.04 input canvas slider
  return ()

printCanvas :: Double -> Element -> Canvas -> Element -> UI ()
printCanvas scale input canvas slider = do

  -- Get the current formula (a String) from the input element
  formula <- get value input

  -- Clear the canvas
  resetCanvas canvas slider
  disableSlider slider

  case readExpr formula of
    Just exp -> 
      path "blue" (points exp scale (canWidth, canHeight)) canvas
      >> pure slider # set style [("cursor","pointer"), ("pointer-events", "auto")]
      >> return ()
    Nothing -> 
      UI.fillText "Invalid formula" (5, canHeight-5) canvas
    
-- converts a pixel x-coordinate to a real x-coordinate
pixToReal :: (Double, Double) -> (Double, Double)
pixToReal x = undefined

-- converts a real y-coordinate to a pixel y-coordinate
realToPix :: (Double, Double) -> Double -> (Double, Double)
realToPix (x, y) scale = (factor * x + canWidth/2, -y * factor + canHeight/2)
  where factor = 1.0 / scale

points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width, height) = [realToPix (x, eval exp x) scale | x <- [(-limit), (-limit) + scale..limit]]
  where limit = scale * fromIntegral width / 2.0 

zoom :: Element -> Canvas -> Element -> UI ()
zoom input canvas slider = do
  v <- get value slider
  let zValue = 0.04 / (read v/100)
  printCanvas zValue input canvas slider
