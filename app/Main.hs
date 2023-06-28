{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Graphics.Gloss qualified as Gloss
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char), KeyState (Down, Up))
import Linear.Metric
import Linear.V2
import Linear.Vector

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

newtype Shape = Circle {_radius :: Coord}

makeLenses ''Shape

data Collider = Collider
  { _position :: V2 Coord,
    _shape :: Shape
  }

makeLenses ''Collider

data Collision = Collision
  { _normal :: V2 Coord,
    _penetration :: Coord
  }

makeLenses ''Collision

data CollisionInfo = CollisionInfo Id Id Collision

makeLenses ''CollisionInfo

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
      _actors = [Actor {_body = Body {_collider = Collider {_position = 0.0, _shape = Circle 50.0}, _velocity = 0.0}, _health = 100.0, _speed = 100.0}, Actor {_body = Body {_collider = Collider {_position = 200.0, _shape = Circle 30.0}, _velocity = 0.0}, _health = 10.0, _speed = 100.0}]
    }

-- Logic

vec2Len :: (Floating a) => V2 a -> a
vec2Len (V2 x y) = sqrt (x * x + y * y)

vec2NormOrZero :: (Floating a, Ord a) => V2 a -> V2 a
vec2NormOrZero vec =
  let len = vec2Len vec
   in if abs len < 0.001 then 0 else vec ^/ len

checkCollision :: Collider -> Collider -> Maybe Collision
checkCollision col_a col_b = case (col_a ^. shape, col_b ^. shape) of
  (Circle radius_a, Circle radius_b) ->
    let delta = col_b ^. position - col_a ^. position
        dist = vec2Len delta
        penetration = radius_a + radius_b - dist
     in if penetration < 0
          then Nothing
          else
            Just
              ( Collision
                  { _normal = vec2NormOrZero delta,
                    _penetration = penetration
                  }
              )

updateControls :: Time -> WorldM ()
updateControls _deltaTime = do
  keys <- use pressedKeys
  let left = checkKey keysLeft keys
  let right = checkKey keysRight keys
  let up = checkKey keysUp keys
  let down = checkKey keysDown keys
  let toDir b = if b then 1 else -1
  let dir = V2 (toDir right - toDir left) (toDir up - toDir down)
  player . inputDir .= dir

controlPlayer :: Time -> WorldM ()
controlPlayer _deltaTime = do
  playe <- use player
  let playerId = playe ^. actorId
  playerActor <- preuse $ actors . ix playerId
  forM_ playerActor $ \playerActor -> do
    let targetVel = (playe ^. inputDir) ^* (playerActor ^. speed)
    actors . ix playerId . body . velocity .= targetVel

moveBody :: Time -> State Body ()
moveBody deltaTime = do
  vel <- use velocity
  collider . position += vel ^* deltaTime

moveWorld :: Time -> WorldM ()
moveWorld deltaTime = (actors . traverse . body) %= execState (moveBody deltaTime)

pairs :: [a] -> [(a, a)]
pairs xs = concatMap pairFirst (tails xs)
  where
    pairFirst :: [a] -> [(a, a)]
    pairFirst [] = []
    pairFirst (x : xs) = fmap (x,) xs

checkCollisions :: WorldM [CollisionInfo]
checkCollisions = do
  world <- get
  let colliders = itoList (world ^.. actors . traverse . body . collider)
  return $ mapMaybe collide (pairs colliders)
  where
    collide :: ((Int, Collider), (Int, Collider)) -> Maybe CollisionInfo
    collide ((idA, colA), (idB, colB)) =
      let mkInfo = CollisionInfo idA idB
       in fmap mkInfo (checkCollision colA colB)

resolveCollision :: CollisionInfo -> WorldM ()
resolveCollision (CollisionInfo idA idB (Collision normal penetration)) = do
  bodyA <- preuse $ actors . ix idA . body
  forM_ bodyA $ \bodyA -> do
    bodyB <- preuse $ actors . ix idB . body
    forM_ bodyB $ \bodyB -> do
      let relativeVel = (bodyA ^. velocity) - (bodyB ^. velocity)
      let dotVel = relativeVel `dot` normal
      let updateA = do
            collider . position -= normal ^* (penetration / 2)
            velocity -= normal ^* dotVel ^* 0.5
      let updateB = do
            collider . position += normal ^* (penetration / 2)
            velocity += normal ^* dotVel ^* 0.5
      actors . ix idA . body %= execState updateA
      actors . ix idB . body %= execState updateB

updateWorld :: Float -> WorldM ()
updateWorld deltaTime = do
  updateControls deltaTime
  controlPlayer deltaTime
  moveWorld deltaTime
  collisions <- checkCollisions
  forM_ collisions resolveCollision

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

-- Rect _width _height -> Gloss.blank -- TODO

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
