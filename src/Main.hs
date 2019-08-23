{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Radius = Float
type Position = (Float, Float)

-- | Window dimensions
width, height :: Int
width = 400
height = 400

windowPosition :: (Int, Int)
windowPosition = (400, 400)

ballRadius :: Radius
ballRadius = 10

-- | Paddle dimensions
paddleWidth, paddleHeight :: Float
paddleWidth = 26
paddleHeight = 86

wallThickness :: Float
wallThickness = 10

data ColorTheme = ColorTheme
  { ballColor, paddleColor, leftPaddleBorderColor, rightPaddleBorderColor
  , wallColor, bgColor :: Color
  } deriving Show

defaultTheme :: ColorTheme
defaultTheme = ColorTheme
  { ballColor = dark red
  , paddleColor = light (light blue)
  , leftPaddleBorderColor = orange
  , rightPaddleBorderColor = rose
  , wallColor = greyN 0.5
  , bgColor = black
  }

fps :: Int
fps = 60

-- | How far to move the paddle per keypress event
movementStep :: Float
movementStep = 5

-- | Game state
data PongGame = Game
  { ballLoc :: Position  -- ^ Pong ball (x, y) location.
  , ballVel :: Position  -- ^ Pong ball (x, y) velocity.
  , playerR :: Position  -- ^ Right player paddle position.
                         --   Zero is the middle of the screen.
  , playerL :: Position  -- ^ Left player paddle position.
  , score_L :: Int
  , score_R :: Int
  , paused :: Bool       -- ^ Is the game currently paused?
  , buttons :: ButtonStatus
  , theme :: ColorTheme
  , windowSize :: (Int, Int)
  } deriving Show

-- | Held-down status of varions action buttons
data ButtonStatus = Buttons
  { playerL_up, playerL_down, playerR_up, playerR_down :: KeyState
  } deriving Show

initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (30, -30)
  , playerR = ( fromIntegral width/2 - paddleWidth/2 - 10, -80)
  , playerL = (-fromIntegral width/2 + paddleWidth/2 + 10, -80)
  , score_L = 0
  , score_R = 0
  , paused = False
  , buttons = Buttons
              { playerL_up = Up
              , playerL_down = Up
              , playerR_up = Up
              , playerR_down = Up
              }
  , theme = defaultTheme
  , windowSize = (width, height)
  }

-- | Update ball position using its velocity
moveBall :: Float    -- ^ Seconds since last update
         -> PongGame -- ^ Initial game state
         -> PongGame -- ^ Updated game state
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old location and velocity
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New location
    x' = x + vx * seconds
    y' = y + vy * seconds

render :: PongGame  -- ^ Game state to render
       -> Picture   -- ^ A picture of this game state
render Game{ballLoc, playerL, playerR, score_L, score_R, theme
           , windowSize = (wsizeX, wsizeY)
           } =
  scale (fromIntegral wsizeX / fromIntegral width) (fromIntegral wsizeY / fromIntegral height) $
  pictures [ ball
           , walls
           , mkPaddle (rightPaddleBorderColor theme) playerR
           , mkPaddle (leftPaddleBorderColor theme) playerL
           , scoreText_L
           , scoreText_R
           ]
  where
    ball = uncurry translate ballLoc $ color (ballColor theme) (circleSolid ballRadius)

    scoreText_L = translate (-110) (fromIntegral height * 0.3) $ scale 0.3 0.3 $ color red $ text (show score_L)
    scoreText_R = translate   100  (fromIntegral height * 0.3) $ scale 0.3 0.3 $ color red $ text (show score_R)

    mkWall :: Float -> Picture
    mkWall yOffset =
      translate 0 yOffset $
        color (wallColor theme) $
          rectangleSolid (fromIntegral width * 0.9) wallThickness

    -- Top and bottom walls
    walls = pictures [ mkWall (fromIntegral height / 2)
                     , mkWall (-fromIntegral height / 2)
                     ]

    -- Draw a paddle
    mkPaddle :: Color    -- ^ Border color
             -> Position -- ^ Paddle position
             -> Picture
    mkPaddle borderColor (x, y) = pictures
      [ translate x y $ color borderColor $ rectangleSolid paddleWidth paddleHeight
      , translate x y $ color (paddleColor theme) $ rectangleSolid (paddleWidth * 0.75) (paddleHeight * 0.9)
      ]

-- | Update the game: move the ball, detect collisions/bounces/wins, apply button effects
update :: Float    -- ^ Seconds since last frame
       -> PongGame -- ^ Pre-update game state
       -> PongGame -- ^ New game state
update seconds game
  | paused game = game
  | otherwise = (updateScore . paddleBounce . wallBounce . applyButtonActions . moveBall seconds) game

-- | When either player scores, update the scorecard and reset the ball position
updateScore :: PongGame -> PongGame
updateScore game@Game{ballLoc = (x, _), score_R, score_L}
  | x <= -fromIntegral width / 2 = resetBall $ game { score_R = score_R + 1 }
  | x >=  fromIntegral width / 2 = resetBall $ game { score_L = score_L + 1 }
  | otherwise = game

resetBall :: PongGame -> PongGame
resetBall game = game { ballLoc = (0, 0) }

-- | Detect top / bottom wall collision
wallCollision :: Position -> Bool
wallCollision (_, y) = topCollision || bottomCollision
  where
    bottomCollision = y - ballRadius <= -(fromIntegral height / 2) + 5
    topCollision    = y + ballRadius >=  (fromIntegral height / 2) - 5

-- | Bounce the ball when it hits a wall
wallBounce :: PongGame -> PongGame
wallBounce game@Game{ballLoc, ballVel = (vx, vy)} =
  game { ballVel = (vx, vy') }
  where vy' = if wallCollision ballLoc
              then -vy -- Bounce: switch direction
              else vy  -- No change

-- | Bounce the ball when it hits a paddle
paddleBounce :: PongGame -> PongGame
paddleBounce game@Game{ballVel = (vx, vy)} =
  game { ballVel = (vx', vy) }
  where vx' = if paddleCollision game
              then -vx
              else vx

-- | Detect paddle collision
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

-- | Keypress and other events handler
handleEvent :: Event -> PongGame -> PongGame

-- 'r': Reset ball to the center
handleEvent (EventKey (Char 'r') Down _ _) game =
  resetBall game

-- 'p': Pause the game
handleEvent (EventKey (Char 'p') Down _ _) game@Game{paused} =
  game { paused = not paused }

-- Paddle motion keys go into game state to be applied later
handleEvent (EventKey key ks _ _) game@Game{buttons} =
  case key of
    Char 'w' -> game {buttons = buttons {playerL_up = ks}}
    Char 's' -> game {buttons = buttons {playerL_down = ks}}
    SpecialKey KeyUp -> game {buttons = buttons {playerR_up = ks}}
    SpecialKey KeyDown -> game {buttons = buttons {playerR_down = ks}}
    _ -> game

-- Window resized? Stick it in game state.
handleEvent (EventResize xy) game =
  game { windowSize = xy }

-- Ignore un-handled events
handleEvent _ game = game

-- | Apply effects from held-down buttons
applyButtonActions :: PongGame -> PongGame
applyButtonActions = pL_up . pL_dn . pR_up . pR_dn
  -- This whole function is gross; I'll fix it later.
  where
    inBounds y = abs y <= fromIntegral height / 2 - paddleHeight / 2

    pL_up g@Game{playerL = (x, y), buttons = Buttons{playerL_up=Down}}
      | inBounds (y+movementStep) = g{playerL = (x, y+movementStep)}
    pL_up g = g
    pL_dn g@Game{playerL = (x, y), buttons = Buttons{playerL_down=Down}}
      | inBounds (y-movementStep) = g{playerL = (x, y-movementStep)}
    pL_dn g = g
    pR_up g@Game{playerR = (x, y), buttons = Buttons{playerR_up=Down}}
      | inBounds (y+movementStep) = g{playerR = (x, y+movementStep)}
    pR_up g = g
    pR_dn g@Game{playerR = (x, y), buttons = Buttons{playerR_down=Down}}
      | inBounds (y-movementStep) = g{playerR = (x, y-movementStep)}
    pR_dn g = g

start :: PongGame -> IO ()
start game@Game{windowSize, theme} =
  play window (bgColor theme) fps game render handleEvent update
  where window = InWindow "Pong" windowSize windowPosition

main :: IO ()
main = start initialState
