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
backgroundColor = Gloss.black

stepRate :: Int
stepRate = 32

initialWorld :: World
initialWorld = World
    { ballPosition = (0, 0)
    , ballVelocity = (-200, -100)
    , paddleOffset = 0
    , paddleState = Stationary
    }

renderWorld :: World -> Gloss.Picture
renderWorld world = Gloss.pictures
    [ Gloss.circle ballRadius
        |> uncurry Gloss.translate (ballPosition world)
        |> Gloss.color Gloss.white
    , Gloss.rectangleWire paddleWidth paddleHeight
        |> Gloss.translate (-worldWidth / 2 + paddleWidth) (paddleOffset world)
        |> Gloss.color Gloss.white
    ]

handleEvent :: Gloss.Event -> World -> World
handleEvent event world = case event of
    Gloss.EventKey (Gloss.SpecialKey Gloss.KeyUp) Gloss.Down _ _ ->
        world { paddleState = MovingUp }
    Gloss.EventKey (Gloss.SpecialKey Gloss.KeyUp) Gloss.Up _ _ ->
        if paddleState world == MovingUp
        then world { paddleState = Stationary }
        else world
    Gloss.EventKey (Gloss.SpecialKey Gloss.KeyDown) Gloss.Down _ _ ->
        world { paddleState = MovingDown }
    Gloss.EventKey (Gloss.SpecialKey Gloss.KeyDown) Gloss.Up _ _ ->
        if paddleState world == MovingDown
        then world { paddleState = Stationary }
        else world
    _ -> world

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

        o = paddleOffset world
        o' = case paddleState world of
            Stationary -> o
            MovingUp -> o + time * paddleVelocity
            MovingDown -> o - time * paddleVelocity
    in  world
        { ballPosition = (px', py')
        , ballVelocity = (vx', vy')
        , paddleOffset = o'
        }

data World = World
    { ballPosition :: (Float, Float)
    , ballVelocity :: (Float, Float)
    , paddleOffset :: Float
    , paddleState :: PaddleState
    } deriving (Eq, Ord, Read, Show)

data PaddleState
    = Stationary
    | MovingUp
    | MovingDown
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

worldWidth :: (Num a) => a
worldWidth = 1440

worldHeight :: (Num a) => a
worldHeight = 900

ballRadius :: Float
ballRadius = 10

paddleWidth :: Float
paddleWidth = 50

paddleHeight :: Float
paddleHeight = 200

paddleVelocity :: Float
paddleVelocity = 200
