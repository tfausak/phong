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
displayMode = Gloss.FullScreen (worldWidth, worldHeight)

backgroundColor :: Gloss.Color
backgroundColor = Gloss.makeColor 0.5 0.5 0.5 0.0

stepRate :: Int
stepRate = 16

initialWorld :: World
initialWorld = World
    { ballPosition = (0.0, 0.0)
    , ballVelocity = (-20.0, -10.0)
    }

renderWorld :: World -> Gloss.Picture
renderWorld world = Gloss.pictures
    [ Gloss.text (show world)
        |> Gloss.scale 0.2 0.2
        |> Gloss.translate (-700.0) (-420.0)
    , Gloss.circle ballRadius
        |> uncurry Gloss.translate (ballPosition world)
    ]

handleEvent :: Gloss.Event -> World -> World
handleEvent _ world = world

handleStep :: Float -> World -> World
handleStep time world =
    let (px, py) = ballPosition world
        (vx, vy) = ballVelocity world

        px' = px + time * vx
        py' = py + time * vy

        vx' = if px' >= (worldWidth / 2 - ballRadius) || px' <= (ballRadius - worldWidth / 2)
            then -vx else vx
        vy' = if py' >= (worldHeight / 2 - ballRadius) || py' <= (ballRadius - worldHeight / 2)
            then -vy else vy
    in  world
        { ballPosition = (px', py')
        , ballVelocity = (vx', vy')
        }

data World = World
    { ballPosition :: (Float, Float)
    , ballVelocity :: (Float, Float)
    } deriving (Eq, Ord, Read, Show)

worldWidth :: (Num a) => a
worldWidth = 1440

worldHeight :: (Num a) => a
worldHeight = 900

ballRadius :: Float
ballRadius = 10.0
