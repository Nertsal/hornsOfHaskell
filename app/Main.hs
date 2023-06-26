{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Down, Up))

type Id = Int

type Time = Float

type Coord = Float

type Hp = Float

keysLeft :: [Key]
keysLeft = [Char 'a']

keysRight :: [Key]
keysRight = [Char 'd']

checkKey :: [Key] -> Set Key -> Bool
checkKey candidates pressed = any (`Set.member` pressed) candidates

data Player = Player
  { _actorId :: Id,
    _inputDir :: Coord
  }

makeLenses ''Player

data Body = Body
  { _position :: Coord,
    _velocity :: Coord
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

initialState :: World
initialState =
  World
    { _pressedKeys = Set.empty,
      _player = Player {_actorId = 0, _inputDir = 0.0},
      _actors = [Actor {_body = Body {_position = 0.0, _velocity = 0.0}, _health = 100.0, _speed = 100.0}]
    }

-- Logic

updateControls :: Time -> World -> World
updateControls _deltaTime world =
  let keys = _pressedKeys world
      left = checkKey keysLeft keys
      right = checkKey keysRight keys
      toDir b = if b then 1 else -1
      dir = toDir right - toDir left
   in set (player . inputDir) dir world

controlPlayer :: Time -> World -> World
controlPlayer _deltaTime world =
  let playe = world ^. player
      playerId = playe ^. actorId
   in fromMaybe
        world
        ( do
            playerActor <- world ^? actors . ix playerId
            let targetVel = playe ^. inputDir * playerActor ^. speed
            return $ set (actors . ix playerId . body . velocity) targetVel world
        )

moveBody :: Time -> Body -> Body
moveBody deltaTime body = over position (+ body ^. velocity * deltaTime) body

moveBodies :: Time -> [Body] -> [Body]
moveBodies deltaTime = map (moveBody deltaTime)

moveWorld :: Time -> World -> World
moveWorld deltaTime = over (actors . traverse . body) (moveBody deltaTime)

updateWorld :: Float -> World -> World
updateWorld deltaTime = moveWorld deltaTime . controlPlayer deltaTime . updateControls deltaTime

-- Handle event

handleKeyDown :: Key -> World -> World
handleKeyDown key = over pressedKeys (Set.insert key)

handleKeyUp :: Key -> World -> World
handleKeyUp key = over pressedKeys (Set.delete key)

handleWorld :: Event -> World -> World
handleWorld (EventKey key Down _ _) = handleKeyDown key
handleWorld (EventKey key Up _ _) = handleKeyUp key
handleWorld _ = id

-- Draw

drawCircle :: Coord -> Coord -> Picture
drawCircle radius position = color white . translate position 0 $ circleSolid radius

drawActor :: Actor -> Picture
drawActor actor =
  let radius = 100
      pos = actor ^. body . position
   in drawCircle radius pos

drawWorld :: World -> Picture
drawWorld world = pictures (fmap drawActor (_actors world))

main :: IO ()
main = play display background fps initialState drawWorld handleWorld updateWorld
  where
    display = FullScreen
    background = black
    fps = 60
