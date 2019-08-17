module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

type Radius = Float
type Position = (Float, Float)

width, height :: Int
width = 400
height = 400
windowPosition :: (Int, Int)
windowPosition = (400, 400)

ballRadius :: Radius
ballRadius = 10

paddleWidth, paddleHeight :: Float
paddleWidth = 26
paddleHeight = 86

background :: Color
background = black

-- | Number of frames to show per second.
fps :: Int
fps = 60

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: Position  -- ^ Pong ball (x, y) location.
  , ballVel :: Position  -- ^ Pong ball (x, y) velocity.
  , playerR :: Position  -- ^ Right player paddle position.
                         -- Zero is the middle of the screen.
  , playerL :: Position  -- ^ Left player paddle position.
  } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (30, -30)
  , playerR = (120, -80)
  , playerL = (-120, -80)
  }

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Convert a game state into a picture.
render :: PongGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [ball, walls,
            mkPaddle rose $ playerR game,
            mkPaddle orange $ playerL game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = dark red

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall yOffset =
      translate 0 yOffset $
        color wallColor $
          rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall (fromIntegral height / 2), wall (-fromIntegral height / 2)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Position -> Picture
    mkPaddle col (x, y) = pictures
      [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = light (light blue)

-- | Update the game by moving the ball and bouncing off walls.
update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = paddleBounce . wallBounce . moveBall seconds

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision
  where
    topCollision    = y - ballRadius <= -fromIntegral height / 2
    bottomCollision = y + ballRadius >=  fromIntegral height / 2

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    (vx, vy) = ballVel game -- old velocities
    vy' = if wallCollision (ballLoc game)
          then -vy -- update velocity: switch direction
          else vy  -- do nothing: return the old velocity unchanged

-- | Detect a collision with a paddle. Upon collisions,
-- change the velocity of the ball to bounce it off the paddle.
paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
    (vx, vy) = ballVel game
    vx' = if paddleCollision game
          then -vx
          else vx

paddleCollision :: PongGame -> Bool
paddleCollision Game{ballLoc = (x, y), playerR = (playerR_x, playerR_y), playerL = (playerL_x, playerL_y)} = leftCollision || rightCollision
  where
    leftCollision =    x <= rightEdgeOfLeftPaddle
                    && y <= topEdgeOfLeftPaddle
                    && y >= bottomEdgeOfLeftPaddle
    rightCollision = x >= leftEdgeOfRightPaddle
                     && y <= topEdgeOfRightPaddle
                     && y >= bottomEdgeOfRightPaddle
    rightEdgeOfLeftPaddle = playerL_x + (paddleWidth / 2)
    leftEdgeOfRightPaddle = playerR_x - (paddleWidth / 2)
    topEdgeOfLeftPaddle = playerL_y + (paddleHeight / 2)
    bottomEdgeOfLeftPaddle = playerL_y - (paddleHeight / 2)
    topEdgeOfRightPaddle = playerR_y + (paddleHeight / 2)
    bottomEdgeOfRightPaddle = playerR_y - (paddleHeight / 2)

window :: Display
window = InWindow "Pong" (width, height) windowPosition

main :: IO ()
main = simulate window background fps initialState render update
