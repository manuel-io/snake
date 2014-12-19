module Main
where

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid
import qualified System.Random as Random

type Coord = (Int, Int)

data Snake = Snake { head' :: Coord
                   , body' :: [Coord]
                   } deriving (Eq, Show)

data Move = SLeft | SRight | SDown | SUp deriving (Show)

data World = World { nextMove :: Move
                   , snake :: Snake
                   , food :: Coord
                   , score :: Int
                   , random :: Random.StdGen
                   , status :: Int
                   } deriving (Show)

square :: Float
square = 15.0

size :: Float
size = 500.0

main :: IO ()
main = do
  ran <- Random.getStdGen
  play (InWindow "Snake" (round size + 20, round size + 20) (10, 10))
    black
    9
    (World SUp (initSnake) (20, 20) 0 ran 0)
    (drawWorld squares)
    (handleEvent)
    (updateWorld squares)
  where
    initSnake = Snake (10, 10) [(10, 9), (10, 8), (10, 7), (10, 6)]
    squares = round (size/square)

drawBlock :: Coord -> Picture
drawBlock (x, y) =
  polygon [(x', y'), (x' + s, y'), (x' + s, y' + s), (x', y' + s)]
  where
    s = square
    x' = s * (fromIntegral x)
    y' = s * (fromIntegral y)

drawHead :: Coord -> Picture
drawHead coord = Color white $ drawBlock coord

drawBody :: [Coord] -> Picture
drawBody coords = Color white
  $ mconcat
  $ map drawBlock coords

drawSnake :: Snake -> [Picture]
drawSnake snake = [ drawHead $ head' snake
                  , drawBody $ body' snake
                  ] 

drawFood :: Coord -> [Picture]
drawFood coord = [Color red $ drawBlock coord] 

drawGrid :: Int -> [Picture] -> [Picture]
drawGrid squares grid =
  if l < (fromIntegral squares) + 1 then
    drawGrid squares (grid ++
      [ Color red $ Line [(square*l, 0), (square*l, size)]
      , Color red $ Line [(0, square*l), (size, square*l)]
      ]
    )
  else grid
  where l = (fromIntegral $ length grid)/2

drawWorld :: Int -> World -> Picture
drawWorld squares world =
  case (status world) of
    0 ->
      Translate (-size/2) (-size/2)
      $ pictures
      $ (drawGrid squares [])
        ++ drawSnake (snake world)
        ++ drawFood (food world)
    1 -> Translate (-size/2) (-size/2)
      $ Color white
      $ Scale 0.2 0.2
      $ Text ("Score: " ++ (show (score world)))

handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) world =
  world { nextMove = SDown }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) world =
  world { nextMove = SUp }
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) world =
  world { nextMove = SLeft }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) world =
  world { nextMove = SRight }
handleEvent _ world = world

getRandom :: Random.RandomGen g => g -> Int -> (Int, g)
getRandom gen len = Random.randomR (0, len) gen 

updateWorld :: Int -> Float -> World -> World
updateWorld n _ world =
  let
    snake' = makeMove (nextMove world) (snake world) 
  in
    process snake'
  where
    process snake'
      | (head' snake') == (food world) =
        let
          gen = random world
          newBody = (head' snake'):(body' snake')
          snake'' = Snake (head' snake') newBody
          score' = (score world) + 1
          list = [(x, y) | x <- [0..n-1], y <- [0..n-1]]
          (randNumber, newGen) = getRandom gen ((length list)-1)
          food' = list !! randNumber
        in
          world { snake = snake'', score = score', food = food', random = newGen }
      | (head' snake') `elem` (body' snake') = world { status = 1 }
      | fst (head' snake') < 0 = world { status = 1 }
      | fst (head' snake') > n = world { status = 1 }
      | snd (head' snake') < 0 = world { status = 1 }
      | snd (head' snake') > n = world { status = 1 }
      | otherwise = world { snake = snake' } 

moveBody :: Coord -> [Coord] -> [Coord]
moveBody (x, y) body = (x, y):(init body)

makeMove :: Move -> Snake -> Snake
makeMove move (Snake (x,y) b) =
  case move of
    SLeft -> moveLeft
    SRight -> moveRight
    SUp -> moveUp
    SDown -> moveDown
  where
    moveLeft = Snake (x - 1, y) (moveBody (x, y) b)
    moveRight = Snake (x + 1, y) (moveBody (x, y) b)
    moveUp = Snake (x, y + 1) (moveBody (x, y) b)
    moveDown = Snake (x, y - 1) (moveBody (x, y) b)
