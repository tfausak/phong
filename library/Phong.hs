module Phong where

import Flow

import qualified Graphics.Gloss.Interface.Pure.Game as Gloss

main :: IO ()
main = Gloss.play
    displayMode
    backgroundColor
    stepRate
    initialWorld
    renderWorld
    handleEvent
    handleStep

displayMode :: Gloss.Display
displayMode = Gloss.FullScreen (1440, 900)

backgroundColor :: Gloss.Color
backgroundColor = Gloss.makeColor 0.5 0.5 0.5 0.0

stepRate :: Int
stepRate = 1

initialWorld :: World
initialWorld = World
    { ballPosition = (0.0, 0.0)
    }

renderWorld :: World -> Gloss.Picture
renderWorld world = Gloss.pictures
    [ Gloss.text (show world)
    , Gloss.circle ballRadius
        |> uncurry Gloss.translate (ballPosition world)
    ]

handleEvent :: Gloss.Event -> World -> World
handleEvent _ world = world

handleStep :: Float -> World -> World
handleStep _ world = world

data World = World
    { ballPosition :: (Float, Float)
    } deriving (Eq, Ord, Read, Show)

ballRadius :: Float
ballRadius = 10.0
