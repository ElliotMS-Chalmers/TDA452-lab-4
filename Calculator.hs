-- This module provides a GUI for a Graph Drawing Calculator
-- that can optionally be used in Lab 4. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.

import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI
import Expr
import Data.Maybe (isNothing, fromJust)
import Parsing(readsP, parse, digit, oneOrMore)
import Text.Read (readMaybe)

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
     zPrompt <- mkHTML "<i>Zoom</i>"          -- The zoom text
     inputZ  <- mkInput 20 "1.0"              -- The zoom input
     zoom    <- mkButton "Zoom"               -- The zoom button
     diff    <- mkButton "Differentiate"      -- THe differential button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     zooming <- row [pure zPrompt, pure inputZ]
     getBody window #+ [column [pure canvas,pure formula,pure draw, pure zooming, pure zoom,pure diff]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]
     pure inputZ # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)

     on UI.click     zoom   $ \ _ -> readAndDraw input inputZ canvas
     on valueChange' inputZ $ \ _ -> readAndDraw input inputZ canvas

     on UI.click     diff   $ \_  -> differ      input inputZ canvas


     on UI.click     draw   $ \ _ -> readAndDraw input inputZ canvas
     on valueChange' input  $ \ _ -> readAndDraw input inputZ canvas


-- | differ given two elements, a canvas and a UI attempts
-- to perform the differentiation operation on the
-- first element and displays the result on the canvas.
-- If input is invalid, the zero value graph is shown.
differ :: Element -> Element -> Canvas -> UI ()
differ input inputZ canvas =
    do
        formula <- get value input
        let e = readExpr formula

        let readDraw i = readAndDraw i inputZ canvas
        if isNothing e
            then
                readDraw input
            else
                do
                    let expr = fromJust e
                    let string = showExpr $ differentiate expr
                    exps <- mkInput 20 string
                    element input # set UI.value string
                    readDraw exps

-- | readAndDraw, given two elements, a canvas and a UI
-- draws the first input as a graph and
-- prints the function on the canvas.
readAndDraw :: Element -> Element -> Canvas -> UI ()
readAndDraw input s canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     let e = readExpr formula

     scaleIn <- get value s
     let readScale = readMaybe scaleIn :: Maybe Double
     let scale = maybe 0.04 (0.04/) readScale


     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas

     let draw e = path "blue" (points e scale (300,300)) canvas
     let def = draw (num 0)
     maybe def draw e

-------------------------------------------------
-- H

-- | points will calculate all the points of the graph in terms of pixels.
-- The scaling value tells you the ratio between pixels and floating point numbers.
-- The arguments width and height tell you how big the drawing area is.
-- We assume that the origin (0,0) point is in the middle of our drawing area.
points :: Expr -> Double -> (Int,Int) -> [Point]
points e s (w,h) = map center ps
    where
        center (x,y) = (x + width/2, height/2 - y)
        width = fromIntegral w
        height = fromIntegral h
        xs = map pixToReal [-width..width]
        ys = map (eval e) xs
        ps =  zip (map realToPix xs) (map realToPix ys)

        -- converts a pixel x-coordinate to a real x-coordinate
        pixToReal :: Double -> Double
        pixToReal x = x * s
        realToPix :: Double -> Double
        realToPix x = x / s


-------------------------------------------------
-- I
-- see setup
