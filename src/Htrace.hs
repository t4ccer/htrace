module Htrace where

import Control.Monad (forM, forM_, guard)
import Data.List (intercalate)
import Data.Ord (clamp)
import Data.Time.Clock (getCurrentTime)
import GHC.Records (HasField (getField))
import System.IO (IOMode (WriteMode), hClose, hFlush, hPutStr, openFile, stdout)
import System.Random (Random, mkStdGen, randomIO, setStdGen)

class Monad m => MonadRandom m where
  random :: Random a => m a

instance MonadRandom IO where
  random = randomIO

inf :: Double
inf = (10 :: Double) ^ (1000 :: Integer)

newtype Vec3 = Vec3 (Double, Double, Double)
  deriving stock (Show, Eq, Ord)

type Vec3Unit = Vec3

toVec3 :: Double -> Vec3
toVec3 a = Vec3 (a, a, a)

vec3 :: Double -> Double -> Double -> Vec3
vec3 x y z = Vec3 (x, y, z)

vec3Bin :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
vec3Bin f (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = Vec3 (f x1 x2, f y1 y2, f z1 z2)

mmap :: (Double -> Double) -> Vec3 -> Vec3
mmap f (Vec3 (x, y, z)) = Vec3 (f x, f y, f z)

nearZero :: Vec3 -> Bool
nearZero v =
  let s = 1e-8
   in all ((< s) . abs) [v.x, v.y, v.z]

-- * Random

randomRange :: MonadRandom m => Double -> Double -> m Double
randomRange min' max' = (\r -> min' + (max' - min') * r) <$> random

vec3UnitRandom :: MonadRandom m => m Vec3Unit
vec3UnitRandom = vec3Unit <$> randomInUnitSphere

vec3RandomRange :: MonadRandom m => Double -> Double -> m Vec3
vec3RandomRange min' max' =
  vec3 <$> randomRange min' max' <*> randomRange min' max' <*> randomRange min' max'

randomInUnitSphere :: MonadRandom m => m Vec3
randomInUnitSphere = do
  v <- vec3RandomRange -1 1
  if vec3LenSqr v >= 1
    then randomInUnitSphere
    else pure v

(*!) :: Double -> Vec3 -> Vec3
(*!) s = mmap (* s)
infixl 7 *!

(+!) :: Double -> Vec3 -> Vec3
(+!) s = mmap (+ s)
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
vec3LenSqr (Vec3 (x, y, z)) = x * x + y * y + z * z

vec3Len :: Vec3 -> Double
vec3Len = sqrt . vec3LenSqr

-- | Dot product
vec3Dot :: Vec3 -> Vec3 -> Double
vec3Dot (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) = x1 * x2 + y1 * y2 + z1 * z2

-- | Cross product
vec3Cross :: Vec3 -> Vec3 -> Vec3
vec3Cross (Vec3 (x1, y1, z1)) (Vec3 (x2, y2, z2)) =
  vec3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

-- | Convert vector to unit vector
vec3Unit :: Vec3 -> Vec3Unit
vec3Unit v = mmap (/ vec3Len v) v

type Point3 = Vec3
type Color = Vec3

-- | Render color to P3 ppm format
renderColor :: Color -> Int -> String
renderColor c samples =
  -- Divide the color by the number of samples and gamma-correct for gamma=2.0.
  let scale :: Double = 1.0 / (fromIntegral samples)
      r = 256 * (clamp (0, 0.999) (sqrt $ c.x * scale))
      g = 256 * (clamp (0, 0.999) (sqrt $ c.y * scale))
      b = 256 * (clamp (0, 0.999) (sqrt $ c.z * scale))
   in mconcat
        [ show (floor @Double @Int r)
        , " "
        , show (floor @Double @Int g)
        , " "
        , show (floor @Double @Int b)
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

data MaterialRecord = MaterialRecord
  { attenuation :: Color
  , scattered :: Ray
  }

newtype Material m = Material {unMaterial :: Ray -> HitRecord -> m (Maybe MaterialRecord)}

lambertian :: MonadRandom m => Color -> Material m
lambertian col = Material $ \_rayIn hitRec -> do
  randomUnitVec <- vec3UnitRandom
  let scatterDir' = hitRec.normal + randomUnitVec
  -- Catch degenerate scatter direction
  let scatterDir = if nearZero scatterDir' then hitRec.normal else scatterDir'
  pure $
    Just
      MaterialRecord
        { attenuation = col
        , scattered = Ray hitRec.p scatterDir
        }

mkHitRecord :: Ray -> Double -> Point3 -> Vec3 -> HitRecord
mkHitRecord ray t p outwardNormal =
  let (frontFace, normal) = getFaceNormal ray outwardNormal
   in HitRecord
        { p = p
        , t = t
        , normal = normal
        , frontFace = frontFace
        -- , mat = mat
        }

data HitRecord = HitRecord
  { p :: Point3
  , normal :: Vec3
  , t :: Double
  , frontFace :: Bool
  }

newtype Hittable m = Hittable {unHittable :: Ray -> Double -> Double -> Maybe (HitRecord, Material m)}

instance Semigroup (Hittable m) where
  Hittable h1 <> Hittable h2 = Hittable $ \ray tMin tMax -> do
    let r1' = h1 ray tMin tMax
        r2' = h2 ray tMin tMax
    case (r1', r2') of
      (Just r1, Just r2) -> pure $ if (fst r1).t < (fst r2).t then r1 else r2
      (Nothing, Just r2) -> pure r2
      (r1, Nothing) -> r1

instance Monoid (Hittable m) where
  mempty = Hittable $ \_ _ _ -> Nothing

sphere :: MonadRandom m => Point3 -> Double -> Material m -> Hittable m
sphere center radius mat = Hittable $ \ray tMin tMax -> do
  let oc :: Vec3 = ray.origin - center
      a = vec3LenSqr ray.direction
      halfB = vec3Dot oc ray.direction
      c = (vec3LenSqr oc) - (radius * radius)
      discriminant = halfB * halfB - a * c
  guard (discriminant >= 0)
  let sqrtd = sqrt discriminant
  let root = (-halfB - sqrtd) / a
  newRoot <-
    if root < tMin || tMax < root
      then
        let root' = (-halfB - sqrtd) / a
         in if root' < tMin || tMax < root'
              then Nothing
              else pure root'
      else pure root
  let t = newRoot
      p = rayAt t ray
      outwardNormal = (p - center) / (toVec3 radius)
  pure $ (mkHitRecord ray t p outwardNormal, mat)

-- | Convert ray to color
rayColor :: MonadRandom m => Ray -> Hittable m -> Int -> m Color
rayColor _ _ 0 = pure 0
rayColor ray (Hittable world) maxDepth =
  case world ray 0.001 inf of
    Just (rec', Material matFunc) -> do
      mat' <- matFunc ray rec'
      case mat' of
        Just mat -> do
          refl <- rayColor (mat.scattered) (Hittable world) (maxDepth - 1)
          pure $ mat.attenuation * refl
        Nothing -> pure 0
    Nothing ->
      let unitDir = vec3Unit ray.direction
          tSky :: Vec3 = toVec3 (0.5 * (unitDir.y + 1))
       in pure $ (1.0 - tSky) * (vec3 1.0 1.0 1.0) + tSky * (vec3 0.5 0.7 1.0)

data Camera = Camera
  { camAspectRatio :: Double
  , camViewportHeight :: Double
  , camFocalLength :: Double
  , camOrigin :: Point3
  }

camViewportWidth :: Camera -> Double
camViewportWidth cam = (camAspectRatio cam) * (camViewportHeight cam)

camHorizontal :: Camera -> Vec3
camHorizontal cam = vec3 (camViewportWidth cam) 0 0

camVertical :: Camera -> Vec3
camVertical cam = vec3 0 (camViewportHeight cam) 0

camLowerLeftCorner :: Camera -> Point3
camLowerLeftCorner cam =
  (camOrigin cam) - (camHorizontal cam / 2) - (camVertical cam / 2) - (vec3 0 0 (camFocalLength cam))

getRay :: Camera -> Double -> Double -> Ray
getRay cam u v =
  Ray
    (camOrigin cam)
    ((camLowerLeftCorner cam) + u *! (camHorizontal cam) + v *! (camVertical cam) - (camOrigin cam))

run :: IO ()
run = do
  -- Poor mans ReaderT
  let gen = mkStdGen 42
  setStdGen gen
  fname <- intercalate "-" . take 2 . words . show <$> getCurrentTime

  h <- openFile ("out/" <> fname) WriteMode

  -- Image
  let aspectRatio :: Double = 16 / 9
      imageHeight :: Int = floor (fromIntegral imageWidth / aspectRatio)
      imageWidth :: Int = 400
      samples = 128
      maxDepth = 48
  -- imageWidth :: Int = 4096
  -- samples = 512
  -- maxDepth = 256;

  -- Camera
  let cam = Camera aspectRatio 2 1 0

  -- World
  let world =
        mconcat
          [ sphere (vec3 -1 0 -1) 0.5 (lambertian $ vec3 0.3 0 0)
          , sphere (vec3 1 0 -1) 0.25 (lambertian $ vec3 0 0 0.3)
          , sphere (vec3 0 -100.5 -1) 100 (lambertian $ vec3 0 0.3 0)
          ]

  -- Render

  hPutStr h $ mconcat ["P3\n", show imageWidth, " ", show imageHeight, "\n255\n"]
  forM_ [(imageHeight - 1), (imageHeight - 2) .. 0] $ \j -> do
    putStr $ "\rScanlines remaining: " <> show j <> " "
    hFlush stdout
    forM_ [0 .. (imageWidth - 1)] $ \i -> do
      pixelColor <- fmap sum $ forM [1 .. samples] $ \_ -> do
        ur <- randomIO
        vr <- randomIO
        let u :: Double = (fromIntegral i + ur) / (fromIntegral (imageWidth - 1))
            v :: Double = (fromIntegral j + vr) / (fromIntegral (imageHeight - 1))
            ray = getRay cam u v
        rayColor ray world maxDepth
      hPutStr h $ renderColor pixelColor samples
  hFlush h
  hClose h
  putStrLn "Done!"
