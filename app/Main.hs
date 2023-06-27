{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State.Lazy
import Data.Set (Set)
import Data.Set qualified as Set
import Graphics.Gloss qualified as Gloss
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Down, Up))
import Linear.V2

type Id = Int

type Time = Float

type Coord = Float

type Hp = Float

keysLeft :: [Key]
keysLeft = [Char 'a']

keysRight :: [Key]
keysRight = [Char 'd']

keysUp :: [Key]
keysUp = [Char 'w']

keysDown :: [Key]
keysDown = [Char 's']

checkKey :: [Key] -> Set Key -> Bool
checkKey candidates pressed = any (`Set.member` pressed) candidates

data Player = Player
  { _actorId :: Id,
    _inputDir :: V2 Coord
  }

makeLenses ''Player

data Shape = Circle Coord | Rect Coord Coord

makeLenses ''Shape

data Collider = Collider
  { _position :: V2 Coord,
    _shape :: Shape
  }

makeLenses ''Collider

data Body = Body
  { _collider :: Collider,
    _velocity :: V2 Coord
  }

makeLenses ''Body

data Actor = Actor
  { _body :: Body,
    _health :: Hp,
    _speed :: Coord
  }

makeLenses ''Actor

data World = World
  { _pressedKeys :: Set Key,
    _player :: Player,
    _actors :: [Actor]
  }

makeLenses ''World

type WorldM a = State World a

initialState :: World
initialState =
  World
    { _pressedKeys = Set.empty,
      _player = Player {_actorId = 0, _inputDir = 0.0},
      _actors = [Actor {_body = Body {_collider = Collider {_position = 0.0, _shape = Circle 50.0}, _velocity = 0.0}, _health = 100.0, _speed = 100.0}]
    }

-- Logic

updateControls :: Time -> WorldM ()
updateControls _deltaTime = do
  keys <- use pressedKeys
  let left = checkKey keysLeft keys
  let right = checkKey keysRight keys
  let up = checkKey keysUp keys
  let down = checkKey keysDown keys
  let toDir b = if b then 1 else -1
  let dir = V2 (toDir right - toDir left) (toDir up - toDir down)
  (player . inputDir) .= dir

controlPlayer :: Time -> WorldM ()
controlPlayer _deltaTime = do
  playe <- use player
  let playerId = playe ^. actorId
  playerActor <- preuse $ actors . ix playerId
  forM_ playerActor $ \playerActor -> do
    let targetVel = fmap (* playerActor ^. speed) (playe ^. inputDir)
    (actors . ix playerId . body . velocity) .= targetVel

moveBody :: Time -> State Body ()
moveBody deltaTime = do
  vel <- use velocity
  collider . position += fmap (* deltaTime) vel

moveWorld :: Time -> WorldM ()
moveWorld deltaTime = (actors . traverse . body) %= execState (moveBody deltaTime)

updateWorld :: Float -> WorldM ()
updateWorld deltaTime = do
  updateControls deltaTime
  controlPlayer deltaTime
  moveWorld deltaTime

updateWorldPure :: Float -> World -> World
updateWorldPure deltaTime = execState (updateWorld deltaTime)

-- Handle event

handleKeyDown :: Key -> WorldM ()
handleKeyDown key = pressedKeys %= Set.insert key

handleKeyUp :: Key -> WorldM ()
handleKeyUp key = pressedKeys %= Set.delete key

handleWorld :: Event -> WorldM ()
handleWorld (EventKey key Down _ _) = handleKeyDown key
handleWorld (EventKey key Up _ _) = handleKeyUp key
handleWorld _ = return ()

handleWorldPure :: Event -> World -> World
handleWorldPure event = execState (handleWorld event)

-- Draw

drawCircle :: Coord -> V2 Coord -> Gloss.Picture
drawCircle radius (V2 x y) = Gloss.color Gloss.white . Gloss.translate x y $ Gloss.circleSolid radius

drawCollider :: Collider -> Gloss.Picture
drawCollider collider =
  let pos = collider ^. position
   in case collider ^. shape of
        Circle radius -> drawCircle radius pos
        Rect _width _height -> Gloss.blank -- TODO

drawActor :: Actor -> Gloss.Picture
drawActor actor = drawCollider (actor ^. body . collider)

drawWorld :: World -> Gloss.Picture
drawWorld world = Gloss.pictures (fmap drawActor (_actors world))

main :: IO ()
main = Gloss.play display background fps initialState drawWorld handleWorldPure updateWorldPure
  where
    display = Gloss.FullScreen
    background = Gloss.black
    fps = 60
