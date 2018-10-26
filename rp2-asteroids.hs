{-# LANGUAGE OverloadedStrings #-}

import Haste (convert, fromJSString, setTimer, Interval(Once))
import Haste.DOM.JSString (getProp)
import Haste.Events
import Haste.Graphics.Canvas

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.List (partition, foldl')
import Data.Maybe (mapMaybe)
import System.Random (randomRIO, Random(random,randomR))


{- Preparations -}

-- | Returns the size of the canvas in the form @(width, height)@.
-- (This will (apparently) only be accurate if the size of the canvas
-- was specified as html attributes and not modified afterwards ...)
--
getCanvasSize :: Canvas -> IO (Int, Int)
getCanvasSize c = do
    Just w <- getProp c "width" <&> fromJSString
    Just h <- getProp c "height" <&> fromJSString
    return $ (w, h)

-- | Type for keys
data Key = KLeft | KUp | KRight | KSpace | KOTHER

-- | Translate keycodes to the Key type
toKey :: Int -> Key
toKey 37 = KLeft
toKey 38 = KUp
toKey 39 = KRight
toKey 32 = KSpace
toKey _  = KOTHER


{- Main part -}

data Direction = Clockwise | Counterclockwise

data Status = Running | GameOver

data GameState = GameState
    { canvas       :: Canvas
    , canvasWidth  :: Double
    , canvasHeight :: Double
    , gameStatus   :: Status
    , ship         :: Object
    , shipBoost    :: Bool
    , shipRotate   :: Maybe Direction
    , shipShoot    :: Bool
    , obstacles    :: [Object]
    , shots        :: [Shot]
    }

data Object = Object
    { position :: Point    -- i.e. (Double,Double)
    , angle    :: Angle    -- i.e. Double
    , velocity :: Vector   -- i.e. (Double,Double)
    }

data Shot = Shot
    { shotObj  :: Object
    , lifetime :: Int    -- in milliseconds
    }


main :: IO ()
main = do
    Just game_canvas <- getCanvasById "game-canvas"
    c_dims@(c_width, c_height) <- getCanvasSize game_canvas <&>
        (\ (w,h) -> (float w, float h))
    let start_position = (c_width/2, c_height/2)
    
    initial_obstacles <- generateObstacles c_dims start_position
    game_state_p <- newIORef $ GameState
        { canvas       = game_canvas
        , canvasWidth  = c_width
        , canvasHeight = c_height
        , gameStatus   = Running
        , ship         = Object
                            { position = start_position
                            , angle    = 0
                            , velocity = (0,0) }
        , shipBoost    = False
        , shipRotate   = Nothing
        , shipShoot    = False
        , obstacles    = initial_obstacles
        , shots        = []
        }
    
    let change = modifyIORef' game_state_p
    
    window `onEvent` KeyDown $ \ KeyData {keyCode = code} ->
        case toKey code of
            KUp    -> change $ \ gs -> gs { shipBoost = True }
            KLeft  -> change $ \ gs -> gs { shipRotate = Just Counterclockwise }
            KRight -> change $ \ gs -> gs { shipRotate = Just Clockwise }
            KSpace -> change $ \ gs -> gs { shipShoot = True }
            _      -> return ()
    
    window `onEvent` KeyUp $ \ KeyData {keyCode = code} ->
        case toKey code of
            KUp    -> change $ \ gs -> gs { shipBoost = False }
            KLeft  -> change $ \ gs -> gs { shipRotate = Nothing }
            KRight -> change $ \ gs -> gs { shipRotate = Nothing }
            _      -> return ()
    
    runGame game_state_p
    
  where
    float = convert :: Int -> Double

runGame :: IORef GameState -> IO ()
runGame gs_p = do
    modifyIORef' gs_p (checkCollisions . addShot . stepObjects)
    readIORef gs_p >>= \ gs -> do
        render (canvas gs) $ do
            drawShip (ship gs)
            mapM_ drawObstacle (obstacles gs)
            mapM_ (drawShot . shotObj) (shots gs)
        
        case gameStatus gs of
            Running  -> setTimer (Once 10) (runGame gs_p) >> return ()
            GameOver -> return ()

-- | Randomly generate some obstacle objects. A neighbourhood of the point
-- given as (second) argument will be kept empty.
generateObstacles :: (Double, Double)  -- ^ canvas size (width, height)
                  -> Point             -- ^ avoid this point
                  -> IO [Object]
generateObstacles canvas_size avoid_p = do
    count <- randomRIO (1,10 :: Int)
    objs  <- sequence $ replicate count (randomRIO (lo,hi))
    return $ map (push_away_from avoid_p) objs
  where
    lo = Object { position = 0, angle = 0, velocity = 0 }
    hi = Object { position = canvas_size, angle = 2*pi, velocity = (1,1) }
    
    -- This might produce "illegal" coordinates, so for the moment we
    -- rely on 'stepObjects' to fix this ...
    push_away_from p o =
        let o_pos    = position o
            p_to_o   = o_pos - p
            dist_sqr = let (x,y) = p_to_o in x^2+y^2
            
            too_close = dist_sqr < ( 30 {- ship "radius"     -} +
                                     40 {- obstacle "radius" -} +
                                     80 {- safty margin      -}   )^2
            
            p_to_o_unit = let f = 1/(sqrt dist_sqr) in (f,f) * p_to_o
            o_pos' = o_pos + if too_close
                             then 150 * p_to_o_unit
                             else 0
            
        in o{position = o_pos'}

stepObjects :: GameState -> GameState
stepObjects gs = gs { ship      = s''
                    , obstacles = os'
                    , shots     = shs' }
  where
    s   = ship gs
    s'  = moveAndWrapObject gs s
    s'' = rotateShip gs (accelerateShip gs s')
    
    os  = obstacles gs
    os' = map (moveAndWrapObject gs) os
    
    shs  = shots gs
    shs' = mapMaybe delete_or_step shs
    delete_or_step sh = if lifetime sh <= 0
        then Nothing
        else Just $ sh { shotObj  = moveAndWrapObject gs (shotObj sh)
                       , lifetime = lifetime sh - 10 }

moveAndWrapObject :: GameState -> Object -> Object
moveAndWrapObject gs = wrapObject gs . moveObject

moveObject :: Object -> Object
moveObject o = o{ position = position o + velocity o }

wrapObject :: GameState -> Object -> Object
wrapObject gs o = o { position = (x'',y'')
                    , angle    = phi'
                    , velocity = v' }
  where
    (x, y)     = position o
    (c_w, c_h) = (canvasWidth gs, canvasHeight gs)
    
    (x', flip_x) = x `confineTo` c_w
    (y', flip_y) = y `confineTo` c_h
    
    -- note, that we assume (for simplicity) that coordinates
    -- are off by at most the maximal value;
    -- the more general case would require a more complex
    -- implemenation ...
    a `confineTo` max_a | a < 0      = (a + max_a, True)
                        | a > max_a  = (a - max_a, True)
                        | otherwise  = (a, False)
    
    x'' = if flip_y then c_w - x' else x'
    y'' = if flip_x then c_h - y' else y'
    
    phi  = angle o
    phi' = case (flip_x, flip_y) of
        (True,  True ) -> pi + phi
        (True,  False) -> negate phi
        (False, True ) -> pi - phi
        (False, False) -> phi
    
    (vx,vy) = velocity o
    v' = case (flip_x, flip_y) of
        (True,  True ) -> negate (vx, vy)
        (True,  False) -> (vx, negate vy)
        (False, True ) -> (negate vx, vy)
        (False, False) -> (vx, vy)

rotateShip :: GameState -> Object -> Object
rotateShip gs o = o { angle = angle o +
    case shipRotate gs of
        Nothing               -> 0
        Just Clockwise        -> 0.07
        Just Counterclockwise -> negate 0.07 }

accelerateShip :: GameState -> Object -> Object
accelerateShip gs o = o { velocity = velocity o +
    case shipBoost gs of
        False -> 0
        True  -> let phi   = angle o
                     accel = 0.15
                 in (accel, accel) * (cos phi, sin phi) }

addShot :: GameState -> GameState
addShot gs = case shipShoot gs of
    False -> gs
    True  -> gs { shipShoot = False
                , shots     = new_shot : shots gs }
  where
    s   = ship gs
    dir = let phi = angle s in (cos phi, sin phi)
    
    new_shot = Shot { shotObj = Object
                        { position = position s + 30*dir
                        , angle    = angle s
                        , velocity = 5*dir }
                    , lifetime = 1000 }

checkCollisions :: GameState -> GameState
checkCollisions = checkShipCollision . checkShotCollisions

checkShipCollision :: GameState -> GameState
checkShipCollision gs = if have_collision
                        then gs{gameStatus = GameOver}
                        else gs
  where
    -- For convex polygons, collision detection is relatively easy.
    -- Thus we decompose the ship's (non-convex!) shape into two
    -- triangles. Those we can probe against our (rectangle shaped)
    -- obstacles (see also 'haveCollisionRT' below).
    ship_ts = shipToTriangles (ship gs)
    obs_rs  = map obstacleToRectangle (obstacles gs)
    
    have_collision = any collides_with_ship obs_rs
    
    -- an obstacles collides with the ship if and only if (at least)
    -- one of the two ship triangles collides with the obstacle
    collides_with_ship o_r = any (haveCollisionRT o_r) ship_ts

checkShotCollisions :: GameState -> GameState
checkShotCollisions gs = gs { obstacles = remaining_obstacles
                            , shots     = remaining_shots }
  where
    -- for simplicity, we only check the shot's center at the moment:
    hits_obstacle sh o =
        haveCollisionRP (obstacleToRectangle o) (position (shotObj sh))
    
    select_obstacles :: Shot -> [Object]
                     -> ([Object], [Object]) -- (hit by shot, not hit)
    select_obstacles sh = partition (sh `hits_obstacle`)
    
    -- go through all shots;
    (remaining_shots, remaining_obstacles) =
        foldl' go ([], obstacles gs) (shots gs)
    -- for each one, identify all obstacles it hits
    go (shs, obs) sh = case select_obstacles sh obs of
        ([], _)    -> (sh:shs, obs)  -- if it hits nothing, keep the shot;
        (_ , obs') -> (shs, obs')    -- otherwise discard the shot and keep
                                     -- only obstacles not hit by the shot


{- Graphics part -}

drawObject :: Picture () -> Object -> Picture ()
drawObject pic o = translate (position o) $ rotate (angle o) pic

drawShip :: Object -> Picture ()
drawShip = drawObject shipPicture

shipPicture :: Picture ()
shipPicture = fill $
    path [(20,0), (-20,20), (-13,0), (-20,-20), (20,0)]

drawObstacle :: Object -> Picture ()
drawObstacle = drawObject obstaclePicture

obstaclePicture :: Picture ()
obstaclePicture = color (RGB 130 130 130) $ fill $
    rect (-25,-25) (25,25)

drawShot :: Object -> Picture ()
drawShot = drawObject shotPicture

shotPicture :: Picture ()
shotPicture = color (RGB 200 0 0) $
    fill $ circle (0,0) 4.0


{- Collision detection part -}

data Triangle  = Triangle Point Point Point
data Rectangle = Rectangle Point Point Point Point
-- the rectangle's corners have to be ordered "as usual"
-- (i.e. every two consecutive points determine an edge
-- and not a diagonal).

shipToTriangles :: Object -> [Triangle]
shipToTriangles o = [Triangle x y z, Triangle x' y' z']
  where
    points1 = [(20,0), (-20,20), (-13,0)]
    points2 = [(20,0), (-13,0), (-20,-20)]
    [x,y,z]    = map (`transformedLike` o) points1
    [x',y',z'] = map (`transformedLike` o) points2

obstacleToRectangle :: Object -> Rectangle
obstacleToRectangle o = Rectangle a b c d
  where
    points = [(-25,-25), (25,-25), (25,25), (-25,25)]
    [a,b,c,d] = map (`transformedLike` o) points

-- | Apply the same transformation to a point that would
-- be necessary to render the object.
-- 
transformedLike :: Point -> Object -> Point
transformedLike p o = translate_ (rotate_ p)
  where
    phi = angle o
    c   = cos phi
    s   = sin phi
    rotate_ (x,y) = (c*x-s*y, s*x+c*y)
    translate_ q  = q + position o

-- | This is the heart of the "ship vs obstacle" collision detection.
-- It is an ad hoc implementation for our special case of a collision
-- detection algorithm for convex polygons based on the so called
-- Separating Axis Theorem.
-- 
haveCollisionRT :: Rectangle -> Triangle -> Bool
haveCollisionRT (Rectangle a b c d) (Triangle x y z) =
    all have_overlap_on axes
  where
    axes = map normal_vec [x-y, x-z, y-z,  a-b, b-c]
    normal_vec (t,t') = (-t',t)
    
    t_vert = [x,y,z]
    r_vert = [a,b,c,d]
    have_overlap_on ax =
        (t_vert `proj_onto` ax) `overlaps` (r_vert `proj_onto` ax)
    
    vs `proj_onto` ax = let values = map (`dot` ax) vs
                        in (minimum values, maximum values)
    (v1,v1') `dot` (v2,v2') = v1*v2 + v1'*v2'
    
    (i,i') `overlaps` (j,j') = j <= i' && j' >= i

-- | "Collision" detection between a rectangle and a point,
-- i.e.: Is the point contained inside of the rectangle?
--
haveCollisionRP :: Rectangle -> Point -> Bool
haveCollisionRP (Rectangle a b c _) p = p_inside_rect
  where
    phi = let (x,y) = b - a in negate (atan2 y x)
    si  = sin phi
    co  = cos phi
    
    translate_ q  = q - a
    rotate_ (x,y) = (co*x-si*y, si*x+co*y)
    transf        = rotate_ . translate_
    
    (bound_x, bound_y) = transf c
    (px, py)           = transf p
    
    minmax bound   = if bound >= 0 then (0, bound) else (bound, 0)
    (min_x, max_x) = minmax bound_x
    (min_y, max_y) = minmax bound_y
    
    p_inside_rect = (min_x <= px) && (px <= max_x) &&
                    (min_y <= py) && (py <= max_y)


{- Random Object elements -}

-- The following allows us to generate random Object elements:
instance Random Object where
    randomR (lo,hi) gen0 = (obj, gen5)
      where
        ((lo_x, lo_y), (hi_x, hi_y)) = (position lo, position hi)
        (x, gen1) = randomR (lo_x,hi_x) gen0
        (y, gen2) = randomR (lo_y,hi_y) gen1
        
        (lo_phi, hi_phi) = (angle lo, angle hi)
        (phi, gen3)      = randomR (lo_phi, hi_phi) gen2
        
        ((lo_vx, lo_vy), (hi_vx, hi_vy)) = (velocity lo, velocity hi)
        (vx, gen4) = randomR (lo_vx, hi_vx) gen3
        (vy, gen5) = randomR (lo_vy, hi_vy) gen4
        
        obj = Object { position = (x,y)
                     , angle    = phi
                     , velocity = (vx,vy) }
    
    random gen = randomR (lo,hi) gen
      where
        lo = Object { position = (0,0)
                    , angle    = 0
                    , velocity = (0,0) }
        hi = Object { position = (1,1)
                    , angle    = 2*pi
                    , velocity = (1,1) }


{- Misc -}

-- copied from Data.Functor
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

-- copied from package NumInstances
lift2 :: (a->u) -> (b->v) -> (a,b) -> (u,v)
lift2 fa fb (a,b) = (fa a, fb b)

instance (Num a, Num b) => Num (a,b) where
    fromInteger n   = (fromInteger n, fromInteger n)
    (a,b) + (a',b') = (a+a',b+b')
    (a,b) - (a',b') = (a-a',b-b')
    (a,b) * (a',b') = (a*a',b*b')
    negate = lift2 negate negate
    abs    = lift2 abs abs
    signum = lift2 signum signum
