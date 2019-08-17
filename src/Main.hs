module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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
                         --   Zero is the middle of the screen.
  , playerL :: Position  -- ^ Left player paddle position.
  , paused :: Bool       -- ^ Is the game currently paused?
  } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (30, -30)
  , playerR = (120, -80)
  , playerL = (-120, -80)
  , paused = False
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
update :: Float -> PongGame -> PongGame
update seconds game =
  if paused game then game
  else (paddleBounce . wallBounce . moveBall seconds) game

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision
  where
    bottomCollision = y - ballRadius <= -(fromIntegral height / 2) + 5
    topCollision    = y + ballRadius >=  (fromIntegral height / 2) - 5

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
paddleCollision Game{ ballLoc = (x, y)
                    , playerR = (playerR_x, playerR_y)
                    , playerL = (playerL_x, playerL_y) } =
  leftCollision || rightCollision
  where
    leftCollision = x - ballRadius <= rightEdgeOfLeftPaddle
                    && y + ballRadius <= topEdgeOfLeftPaddle
                    && y - ballRadius >= bottomEdgeOfLeftPaddle
    rightCollision = x + ballRadius >= leftEdgeOfRightPaddle
                     && y + ballRadius <= topEdgeOfRightPaddle
                     && y - ballRadius >= bottomEdgeOfRightPaddle
    rightEdgeOfLeftPaddle = playerL_x + (paddleWidth / 2)
    leftEdgeOfRightPaddle = playerR_x - (paddleWidth / 2)
    topEdgeOfLeftPaddle = playerL_y + (paddleHeight / 2)
    bottomEdgeOfLeftPaddle = playerL_y - (paddleHeight / 2)
    topEdgeOfRightPaddle = playerR_y + (paddleHeight / 2)
    bottomEdgeOfRightPaddle = playerR_y - (paddleHeight / 2)

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

-- 'r': Reset the ball to the center
handleKeys (EventKey (Char 'r') _ _ _) game =
  game { ballLoc = (0, 0) }

-- Pause on 'p'
handleKeys (EventKey (Char 'p') Down _ _) game = game {paused = not (paused game)}

-- 'w': Left player move up
handleKeys (EventKey (Char 'w') Down _ _) game@Game{playerL = (lx, ly)} =
  game {playerL = (lx, ly+1)}

-- 's': Left player move down
handleKeys (EventKey (Char 's') Down _ _) game@Game{playerL = (lx, ly)} =
  game {playerL = (lx, ly-1)}

-- Down: Right player move down
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game@Game{playerR = (rx, ry)} =
  game {playerR = (rx, ry+1)}

-- Up: Right player move up
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game@Game{playerR = (rx, ry)} =
  game {playerR = (rx, ry-1)}

-- Do nothing for all other events.
handleKeys _ game = game

window :: Display
window = InWindow "Pong" (width, height) windowPosition

main :: IO ()
main = play window background fps initialState render handleKeys update
