{-# LANGUAGE NamedFieldPuns #-}
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

-- | How far to move the paddle per keypress event
movementStep :: Float
movementStep = 5

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: Position  -- ^ Pong ball (x, y) location.
  , ballVel :: Position  -- ^ Pong ball (x, y) velocity.
  , playerR :: Position  -- ^ Right player paddle position.
                         --   Zero is the middle of the screen.
  , playerL :: Position  -- ^ Left player paddle position.
  , paused :: Bool       -- ^ Is the game currently paused?
  , buttons :: ButtonStatus
  } deriving Show

data ButtonStatus = Buttons
  { playerL_up, playerL_down, playerR_up, playerR_down :: KeyState } deriving Show

-- | The starting state for the game of Pong.
initialState :: PongGame
initialState = Game
  { ballLoc = (-10, 30)
  , ballVel = (30, -30)
  , playerR = (120, -80)
  , playerL = (-120, -80)
  , paused = False
  , buttons = Buttons
              { playerL_up = Up
              , playerL_down = Up
              , playerR_up = Up
              , playerR_down = Up
              }
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
update seconds game
  | paused game = game
  | otherwise = (detectWin . paddleBounce . wallBounce . applyButtonActions . moveBall seconds) game

detectWin :: PongGame -> PongGame
detectWin game@Game{ballLoc = (x, _)}
  | x <= -fromIntegral width / 2 = error "RIGHT PLAYER WINS"
  | x >= fromIntegral width / 2 = error "LEFT PLAYER WINS"
  | otherwise = game

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision
  where
    bottomCollision = y - ballRadius <= -(fromIntegral height / 2) + 5
    topCollision    = y + ballRadius >=  (fromIntegral height / 2) - 5

-- | Detect a collision with one of the side walls. Upon collisions,
-- update the velocity of the ball to bounce it off the wall.
wallBounce :: PongGame -> PongGame
wallBounce game@Game{ballVel = (vx, vy)} =
  game { ballVel = (vx, vy') }
  where vy' = if wallCollision (ballLoc game)
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
    leftCollision = x - ballRadius == rightEdgeOfLeftPaddle
                    && y + ballRadius <= topEdgeOfLeftPaddle
                    && y - ballRadius >= bottomEdgeOfLeftPaddle
    rightCollision = x + ballRadius == leftEdgeOfRightPaddle
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
handleKeys (EventKey (Char 'r') Down _ _) game =
  game { ballLoc = (0, 0) }

-- 'p': Pause the game
handleKeys (EventKey (Char 'p') Down _ _) game =
  game { paused = not (paused game) }

-- Keys that can be held down go into Game state
handleKeys (EventKey key ks _ _) game@Game{buttons} =
  case key of
    Char 'w' -> game {buttons = buttons {playerL_up = ks}}
    Char 's' -> game {buttons = buttons {playerL_down = ks}}
    SpecialKey KeyUp -> game {buttons = buttons {playerR_up = ks}}
    SpecialKey KeyDown -> game {buttons = buttons {playerR_down = ks}}
    _ -> game

-- Do nothing for all other events.
handleKeys _ game = game

-- | Apply effects from held-down buttons
applyButtonActions :: PongGame -> PongGame
applyButtonActions game@Game{playerL = (lx, ly), playerR = (rx, ry), buttons}
  -- TODO: handle multiple buttons held together
  | playerL_up   buttons == Down = game {playerL = (lx, ly+movementStep)}
  | playerL_down buttons == Down = game {playerL = (lx, ly-movementStep)}
  | playerR_up   buttons == Down = game {playerR = (rx, ry+movementStep)}
  | playerR_down buttons == Down = game {playerR = (rx, ry-movementStep)}
  | otherwise = game

window :: Display
window = InWindow "Pong" (width, height) windowPosition

main :: IO ()
main = play window background fps initialState render handleKeys update
