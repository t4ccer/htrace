module Htrace where

import Control.Monad (forM_, guard)
import GHC.Records (HasField (getField))
import System.IO (openFile, hPutStr, hClose, hFlush, IOMode (WriteMode))
import Data.Coerce (coerce)

inf :: Double
inf = (10 :: Double) ^ (1000 :: Integer)

newtype Vec3 = Vec3 (Double, Double, Double)
  deriving stock (Show, Eq, Ord)

newtype Vec3Unit = Vec3Unit {unVec3Unit :: Vec3}
  deriving stock (Show, Eq, Ord)

instance HasField "x" Vec3Unit Double where
  getField = getField @"x" . coerce @Vec3Unit @Vec3

instance HasField "y" Vec3Unit Double where
  getField = getField @"y" . coerce @Vec3Unit @Vec3

instance HasField "z" Vec3Unit Double where
  getField = getField @"z" . coerce @Vec3Unit @Vec3

toVec3 :: Double -> Vec3
toVec3 a = Vec3 (a, a, a)

vec3 :: Double -> Double -> Double -> Vec3
vec3 x y z = Vec3 (x, y, z)

vec3Bin :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
vec3Bin f (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = Vec3 (f x1 x2, f y1 y2, f z1 z2)

mmap :: (Double -> Double) -> Vec3 -> Vec3
mmap f (Vec3 (x, y, z)) = Vec3 (f x, f y, f z)

(*!) :: Double -> Vec3 -> Vec3
(*!) s = mmap (*s)
infixl 7 *!

(+!) :: Double -> Vec3 -> Vec3
(+!) s = mmap (+s)
infixl 6 +!
  
instance HasField "x" Vec3 Double where
  getField (Vec3 (x, _, _)) = x

instance HasField "y" Vec3 Double where
  getField (Vec3 (_, y, _)) = y

instance HasField "z" Vec3 Double where
  getField (Vec3 (_, _, z)) = z

instance Num Vec3 where
  (+) = vec3Bin (+)
  (*) = vec3Bin (*)
  fromInteger i = Vec3 (fromInteger i, fromInteger i, fromInteger i)
  abs = mmap abs
  signum = mmap signum
  negate = mmap negate

instance Fractional Vec3 where
  fromRational r = Vec3 (fromRational r, fromRational r, fromRational r)
  (/) = vec3Bin (/)

vec3LenSqr :: Vec3 -> Double
vec3LenSqr (Vec3 (x, y, z)) = x*x + y*y + z*z

vec3Len :: Vec3 -> Double
vec3Len = sqrt . vec3LenSqr

-- | Dot product
vec3Dot :: Vec3 -> Vec3 -> Double
vec3Dot (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = x1*x2 + y1*y2 + z1*z2

-- | Cross product
vec3Cross :: Vec3 -> Vec3 -> Vec3
vec3Cross (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

-- | Convert vector to unit vector
vec3Unit :: Vec3 -> Vec3Unit
vec3Unit v = Vec3Unit $ mmap (/ vec3Len v) v

type Point3 = Vec3
type Color = Vec3

-- | Render color to P3 ppm format
renderColor :: Color -> String
renderColor v = mconcat
  [ show (floor @Double @Int (v.x * 255.999))
  , " "
  , show (floor @Double @Int (v.y * 255.999))
  , " "
  , show (floor @Double @Int (v.z * 255.999))
  , "\n"
  ]

data Ray = Ray
  { origin :: Vec3
  , direction :: Vec3
  }
  deriving stock (Show, Eq, Ord)

rayAt :: Double -> Ray -> Point3
rayAt t ray = ray.origin + (t *! ray.direction)

getFaceNormal :: Ray -> Vec3 -> (Bool, Vec3)
getFaceNormal ray outwardNormal =
  let frontFace = vec3Dot ray.direction outwardNormal < 0
      normal = if frontFace then outwardNormal else -outwardNormal
   in (frontFace, normal)

mkHitRecord :: Ray -> Double -> Point3 -> Vec3 -> HitRecord
mkHitRecord ray t p outwardNormal =
  let (frontFace, normal) = getFaceNormal ray outwardNormal
   in HitRecord
        { p = p
        , t = t
        , normal = normal
        , frontFace = frontFace
        }

data HitRecord = HitRecord
  { p :: Point3
  , normal :: Vec3
  , t :: Double
  , frontFace :: Bool
  }

newtype Hittable = Hittable {unHittable :: Ray -> Double -> Double -> Maybe HitRecord}

instance Semigroup Hittable where
  Hittable h1 <> Hittable h2 = Hittable $ \ray tMin tMax -> do
    let r1' = h1 ray tMin tMax
        r2' = h2 ray tMin tMax
    case (r1', r2') of
      (Just r1, Just r2) -> pure $ if r1.t < r2.t then r1 else r2
      (Nothing, Just r2) -> pure r2
      (r1, Nothing) -> r1

instance Monoid Hittable where
  mempty = Hittable $ \_ _ _ -> Nothing

sphere :: Point3 -> Double -> Hittable
sphere center radius = Hittable $ \ray tMin tMax -> do
  let oc :: Vec3 = ray.origin - center
      a = vec3LenSqr ray.direction
      halfB = vec3Dot oc ray.direction
      c = (vec3LenSqr oc) - (radius*radius)
      discriminant = halfB*halfB - a*c
  guard (discriminant >= 0)
  let sqrtd = sqrt discriminant
  let root = (-halfB - sqrtd) / a
  newRoot <- if root < tMin || tMax < root
             then
               let root' = (-halfB - sqrtd) / a;
               in if root' < tMin || tMax < root'
                  then Nothing
                  else pure root'
             else pure root
  let t = newRoot
      p = rayAt t ray
      outwardNormal = (p - center) / (toVec3 radius)
  pure $ mkHitRecord ray t p outwardNormal

-- | Convert ray to sky color
rayColor :: Ray -> Hittable -> Color
rayColor ray (Hittable world) =
  case world ray 0 inf of
    Just rec' -> 0.5 * (rec'.normal + 1)
    Nothing ->
      let unitDir = vec3Unit ray.direction
          tSky :: Vec3 = toVec3 (0.5 * (unitDir.y + 1))
      in (1.0-tSky)*(vec3 1.0 1.0 1.0) + tSky*(vec3 0.5 0.7 1.0)

run :: IO ()
run = do
  h <- openFile "out.ppm" WriteMode

  -- Image
  let aspectRatio :: Double = 16 / 9
      imageWidth :: Int = 400
      imageHeight :: Int = floor (fromIntegral imageWidth / aspectRatio)

  -- Camera

  let viewportHeight :: Double = 2
      viewportWidth :: Double = viewportHeight * aspectRatio
      focalLength :: Double = 1
      origin :: Vec3 = 0
      horizontal = vec3 viewportWidth 0 0
      vertical = vec3 0 viewportHeight 0
      lowerLeftCorner = origin - horizontal/2 - vertical/2 - (vec3 0 0 focalLength)
  
  -- Render
      
  hPutStr h $ mconcat[ "P3\n", show imageWidth, " ", show imageHeight, "\n255\n"]
  forM_ [(imageHeight - 1), (imageHeight - 2) .. 0] $ \j -> do
    forM_ [0 .. (imageWidth - 1)] $ \i -> do
      let u :: Double = (fromIntegral i) / (fromIntegral (imageWidth - 1))
          v :: Double = (fromIntegral j) / (fromIntegral (imageHeight - 1))
          ray = Ray origin (lowerLeftCorner + (u *! horizontal) + (v *! vertical) - origin)
          world = mconcat
            [ sphere (vec3 -1 0 -1) 0.5
            , sphere (vec3 1 0 -1) 0.25 
            , sphere (vec3 0 -100.5 -1) 100 -- Ground
            ]
          pixelColor = rayColor ray world
      hPutStr h $ renderColor pixelColor
  hFlush h
  hClose h
