module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.ST (ST, newSTRef, readSTRef)
import Data.Array (length, slice)
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), contains, split, stripPrefix, stripSuffix)
import Matrix (Matrix, empty, fromArray, toIndexedArray)
import Partial.Unsafe (unsafePartial)

newtype RotationVector = RotationVector (Matrix Number)
rotationVector :: Array (Array Number) -> RotationVector
rotationVector a = RotationVector (fromMaybe empty (fromArray a))

newtype TransformMatrix = TransformMatrix (Matrix Number)
transformMatrix :: Array (Array Number) -> TransformMatrix
transformMatrix a = TransformMatrix (fromMaybe empty (fromArray a))

class MatrixToString a where
  toString :: a -> String

instance transformMatrixToString :: MatrixToString TransformMatrix where
  toString (TransformMatrix mt) =
    foldl
      (\s n -> s <> ", " <> show n.value)
      ("(" <> show (unsafePartial head $ toIndexedArray mt).value)
      (unsafePartial tail $ toIndexedArray mt)
    <> ")"


toTransformMatrix :: String -> TransformMatrix
toTransformMatrix str = case contains (Pattern "matrix3d(") str of
  true -> do
            let a = foldl (\ar s -> ar <> [fromMaybe 0.0 (fromString s)]) [] $
                  split (Pattern ", ") $
                  fromMaybe "" (stripSuffix (Pattern ")") $
                  fromMaybe "" (stripPrefix (Pattern "matrix3d(") str))
            if length a /= 16
              then
                TransformMatrix empty
              else
                transformMatrix ([slice 0 4 a] <> [slice 4 8 a] <> [slice 8 12 a] <> [slice 12 16 a])
  _ -> transformMatrix [[1.0, 0.0, 0.0, 0.0], [0.0, 1.0, 0.0, 0.0], [0.0, 0.0, 1.0, 0.0], [0.0, 0.0, 0.0, 1.0]]

instance rotationVectorToString :: MatrixToString RotationVector where
  toString (RotationVector v) =
    foldl
      (\s n -> s <> ", " <> show n.value)
      ("(" <> show (unsafePartial head $ toIndexedArray v).value)
      (unsafePartial tail $ toIndexedArray v)
    <> "deg)"


main :: forall e h. Eff (console :: CONSOLE, st :: ST h | e) Unit
main = do
  let t = transformMatrix [[1.0,2.0,3.0,4.0],[5.0,6.0,7.0,8.0],[9.0,10.0,11.0,12.0],[13.0,14.0,15.0,16.0]]
  ref <- newSTRef t
  t1 <- readSTRef ref
  log "You should add some tests."
