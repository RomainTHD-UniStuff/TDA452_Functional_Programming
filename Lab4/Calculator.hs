{- Lab 4B - Calculator.hs
   Date: 08/12/2021
   Authors: *Redacted due to web crawlers*
   Lab group: 27
 -}

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr

-- Fixed sizes for the drawing area
canWidth, canHeight :: Num a => a
canWidth  = 300
canHeight = 300

-- Multiplication factor, minimum and maximum of the zoom slider
sliderFactor, sliderMin, sliderMax :: Int
sliderFactor = 100
sliderMin = 1 * sliderFactor
sliderMax = 3 * sliderFactor

main :: IO ()
main = startGUI defaultConfig setup

 -- Create them user interface elements
setup :: Window -> UI ()
setup window = do
  canvas   <- mkCanvas canWidth canHeight               -- The drawing area
  zoomL    <- mkHTML "Zoom +/-"                         -- The text "Zoom +/-"
  slider   <- mkSlider (sliderMin, sliderMax) sliderMin -- The zoom slider
  zoomV    <- mkHTML "x1.0"                             -- The current zoom value"
  fx       <- mkHTML "<i>f</i>(<i>x</i>)= "             -- The text "f(x)="
  input    <- mkInput 20 ""                             -- The formula input
  reset    <- mkButton "Reset"                          -- The reset button
  draw     <- mkButton "Draw graph"                     -- The draw button
  diff     <- mkButton "Differentiate"                  -- The differentiate button

  -- Add the user interface elements to the page, creating a specific layout
  zoomBar <- row [pure zoomL, pure slider, pure zoomV]
  formula <- row [pure fx, pure input]
  buttons <- row [pure reset, pure draw, pure diff]
  getBody window #+ [column [pure canvas, pure zoomBar, pure formula, pure buttons]]

  -- Styling
  getBody window # set style [("backgroundColor","lightblue"),
                              ("textAlign","center")]
  pure input # set style [("fontSize","14pt")]

  -- Interaction (install event handlers)

  -- Reset the slider, the formula input and the "canvas"
  on UI.click     reset $ \ _ -> do
    resetSlider slider zoomV
    resetCanvas canvas
    pure input # set value ""

  -- Try to read and draw the current formula
  on UI.click     draw  $ \ _ -> readAndDraw input canvas slider zoomV
  on valueChange' input $ \ _ -> readAndDraw input canvas slider zoomV

  -- | If the formula is valid, update the formula input with the differentiate
  --   of the function. Try to read and draw the current formula
  on UI.click     diff  $ \ _ -> do
    formula <- get value input
    case readExpr formula of
      Just exp -> pure input # set value (showExpr (differentiate exp))
      Nothing  -> pure input
    readAndDraw input canvas slider zoomV

  -- Update the zoomV label and print the "canvas" with the new scale value
  on valueChange' slider $ \ _ -> do
    updateSliderValue slider zoomV
    scale <- (0.04/) <$> getSliderValue slider
    printCanvas scale input canvas slider

  -- Init the zoom slider and the "canvas"
  resetSlider slider zoomV
  resetCanvas canvas

-- Reset and disable the zoom slider
resetSlider :: Element -> Element -> UI Element
resetSlider slider zoomV = do
  pure slider # set value (show sliderFactor)
  updateSliderValue slider zoomV
  pure slider # set style [("cursor","not-allowed"), ("pointer-events", "none")]

-- Get the slider value divided by the sliderFactor
getSliderValue :: Element -> UI Double
getSliderValue slider = (/doubleFactor) . read <$> get value slider
  where doubleFactor = fromIntegral sliderFactor

-- Update the zoomV label with the current value of the slider
updateSliderValue :: Element -> Element -> UI Element
updateSliderValue slider zoomV = do
  value <- getSliderValue slider
  let formatValue = 'x' : show value
  pure zoomV # set UI.html formatValue

-- Clear the given "canvas" and print the x-axis and y-axis
resetCanvas :: Canvas -> UI()
resetCanvas canvas = do
  clearCanvas canvas
  path "black" [(0, canHeight/2), (canWidth, canHeight/2)] canvas
  path "black" [(canWidth/2, 0), (canWidth/2, canHeight)] canvas
  UI.fillText "0" (canWidth/2 + 5, canHeight/2 + 12) canvas

-- Converts a real point to a pixel point
realToPix :: (Double, Double) -> Double -> (Double, Double)
realToPix (x, y) factor = (factor * x + canWidth/2, -y * factor + canHeight/2)

-- Calculate all the points of the graph in terms of pixels
points :: Expr -> Double -> (Int,Int) -> [Point]
points exp scale (width, height) = [realToPix (x, eval exp x) factor | x <- xs]
  where xs = [(-limit), (-limit) + scale..limit]
        limit = scale * fromIntegral width / 2.0
        factor = 1.0 / scale

-- Reads the expression from the given input element and draws the graph
-- on the given canvas
printCanvas :: Double -> Element -> Canvas -> Element -> UI ()
printCanvas scale input canvas slider = do

  -- Get the current formula (a String) from the input element
  formula <- get value input

  -- Clear the "canvas"
  resetCanvas canvas

  -- Read the expression and print its graph
  case readExpr formula of
    Just exp -> do
      path "blue" (points exp scale (canWidth, canHeight)) canvas
      pure slider # set style [("cursor","pointer"), ("pointer-events", "auto")]
      return ()
    Nothing ->
      UI.fillText "Invalid formula" (5, canHeight-5) canvas

-- Reset a zoom slider and read and draw the expression from the given input
-- with a scale of 0.04
readAndDraw :: Element -> Canvas -> Element -> Element -> UI ()
readAndDraw input canvas slider zoomV = do
  resetSlider slider zoomV
  printCanvas 0.04 input canvas slider
