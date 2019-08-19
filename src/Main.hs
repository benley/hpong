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

-- | Game colors
ballColor, paddleColor, wallColor, bgColor :: Color

bgColor = black
ballColor = dark red
wallColor = greyN 0.5
paddleColor = light (light blue)

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
  , paused :: Bool       -- ^ Is the game currently paused?
  , buttons :: ButtonStatus
  } deriving Show

-- | Held-down status of varions action buttons
data ButtonStatus = Buttons
  { playerL_up, playerL_down, playerR_up, playerR_down :: KeyState } deriving Show

initialState :: PongGame
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (30, -30)
  , playerR = ( fromIntegral width/2 - paddleWidth/2 - 10, -80)
  , playerL = (-fromIntegral width/2 + paddleWidth/2 + 10, -80)
  , paused = False
  , buttons = Buttons
              { playerL_up = Up
              , playerL_down = Up
              , playerR_up = Up
              , playerR_down = Up
              }
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
render Game{ballLoc, playerL, playerR} =
  pictures [ ball
           , walls
           , mkPaddle rose playerR
           , mkPaddle orange playerL
           ]
  where
    ball = uncurry translate ballLoc $ color ballColor $ circleSolid ballRadius

    -- Top and bottom walls
    mkWall :: Float -> Picture
    mkWall yOffset =
      translate 0 yOffset $
        color wallColor $
          rectangleSolid (fromIntegral width * 0.9) 10

    walls = pictures [ mkWall (fromIntegral height / 2)
                     , mkWall (-fromIntegral height / 2)
                     ]

    -- Draw a paddle
    mkPaddle :: Color    -- ^ Border color
             -> Position -- ^ Paddle position
             -> Picture
    mkPaddle borderColor (x, y) = pictures
      [ translate x y $ color borderColor $ rectangleSolid paddleWidth paddleHeight
      , translate x y $ color paddleColor $ rectangleSolid (paddleWidth * 0.75) (paddleHeight * 0.9)
      ]

-- | Update the game: move the ball, detect collisions/bounces/wins, apply button effects
update :: Float    -- ^ Seconds since last frame
       -> PongGame -- ^ Pre-update game state
       -> PongGame -- ^ New game state
update seconds game
  | paused game = game
  | otherwise = (detectWin . paddleBounce . wallBounce . applyButtonActions . moveBall seconds) game

-- | If someone scores, crash the entire game because fuck it
detectWin :: PongGame -> PongGame
detectWin game@Game{ballLoc = (x, _)}
  | x <= -fromIntegral width / 2 = error "RIGHT PLAYER WINS"
  | x >=  fromIntegral width / 2 = error "LEFT PLAYER WINS"
  | otherwise = game

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

-- | Keypress handler
handleKeys :: Event -> PongGame -> PongGame

-- 'r': Reset ball to the center
handleKeys (EventKey (Char 'r') Down _ _) game =
  game { ballLoc = (0, 0) }

-- 'p': Pause the game
handleKeys (EventKey (Char 'p') Down _ _) game@Game{paused} =
  game { paused = not paused }

-- Paddle motion keys go into game state to be applied later
handleKeys (EventKey key ks _ _) game@Game{buttons} =
  case key of
    Char 'w' -> game {buttons = buttons {playerL_up = ks}}
    Char 's' -> game {buttons = buttons {playerL_down = ks}}
    SpecialKey KeyUp -> game {buttons = buttons {playerR_up = ks}}
    SpecialKey KeyDown -> game {buttons = buttons {playerR_down = ks}}
    _ -> game

-- Ignore all other input
handleKeys _ game = game

-- | Apply effects from held-down buttons
applyButtonActions :: PongGame -> PongGame
applyButtonActions = pL_up . pL_dn . pR_up . pR_dn
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

window :: Display
window = InWindow "Pong" (width, height) windowPosition

main :: IO ()
main = play window bgColor fps initialState render handleKeys update
