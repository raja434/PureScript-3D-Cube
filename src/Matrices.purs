module Matrices where

import Prelude

import Data.Array (index, length, updateAt, zipWith)
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), contains, split, stripPrefix, stripSuffix)
import LinearAlgebra.Matrix (Matrix, column, element, fromArray, identity, rows)
import LinearAlgebra.Matrix (multiply) as M
import Math (abs, atan, cos, pi, sin, sqrt)


newtype RotationVector = RotationVector (Matrix Number)

rotationVector :: Array Number -> RotationVector
rotationVector a
  | length a == 4 = RotationVector (fromMaybe (identity 1) (fromArray 4 1 a))
  | otherwise = noRotation

noRotation :: RotationVector
noRotation = RotationVector (fromMaybe (identity 1)
  (fromArray 4 1 [0.0, 0.0, 0.0, 0.0]))


newtype TransformMatrix = TransformMatrix (Matrix Number)

transformMatrix :: Array Number -> TransformMatrix
transformMatrix a
  | length a == 16 = TransformMatrix (fromMaybe (identity 1) (fromArray 4 4 a))
  | otherwise = noTransformation

noTransformation :: TransformMatrix
noTransformation = TransformMatrix (fromMaybe (identity 1)
  (fromArray 4 4 [1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0]))


class MatrixToString a where
  toString :: a -> String

instance transformMatrixToString :: MatrixToString TransformMatrix where
  toString (TransformMatrix mt) =
    "(" <>
    (fromMaybe "" $ stripPrefix (Pattern ", ") $
      foldl (\s vec -> s <> foldl (\s' n -> s' <> ", " <> show n) "" vec) "" (rows mt))
    <> ")"

instance rotationVectorToString :: MatrixToString RotationVector where
  toString (RotationVector v) =
    "("
    <> (fromMaybe "" $ stripPrefix (Pattern ", ") $
          foldl (\s vec -> s <> foldl (\s' n -> s' <> ", " <> show n) "" vec) "" (rows v))
    <> "deg)"


toTransformMatrix :: String -> TransformMatrix
toTransformMatrix str
  | contains (Pattern "matrix3d(") str = do
      let a = foldl (\ar s -> ar <> [fromMaybe 0.0 (fromString s)]) [] $
            split (Pattern ", ") $
            fromMaybe "" (stripSuffix (Pattern ")") $
            fromMaybe "" (stripPrefix (Pattern "matrix3d(") str))
      if length a /= 16
        then
          noTransformation
        else
          transformMatrix a
  | otherwise = noTransformation


multiply :: TransformMatrix -> RotationVector -> RotationVector
multiply (TransformMatrix mt) (RotationVector v) = RotationVector (M.multiply mt v)

angle :: RotationVector -> Number
angle (RotationVector m) = fromMaybe 0.0 $ element 3 0 m

sum :: Array (RotationVector) -> RotationVector
sum vs = foldl (\acc v -> add acc v) noRotation vs
  where
    add :: RotationVector -> RotationVector -> RotationVector
    add (RotationVector m1) (RotationVector m2) = do
      let a = zipWith (+) (column 0 m1) (column 0 m2)
      let x = fromMaybe 0.0 (index a 0)
      let y = fromMaybe 0.0 (index a 1)
      rotationVector $ fromMaybe [] (updateAt 3 (sqrt $ x * x + y * y) a)

average :: Array (RotationVector) -> RotationVector
average vs = do
  let (RotationVector sum) = sum vs
  let s = (fromMaybe 0.0 (element 3 0 sum)) / toNumber (length vs)
  changeSpeed s (RotationVector sum)

changeSpeed :: Number -> RotationVector -> RotationVector
changeSpeed s (RotationVector v) = do
  let x = fromMaybe 0.0 (element 0 0 v)
  let y = fromMaybe 0.0 (element 1 0 v)
  let a = if y == 0.0 then 0.0 else if x == 0.0 then pi / 2.0 else atan (abs(y / x))
  rotationVector [
    (s * cos a) * (if x < 0.0 then -1.0 else 1.0),
    (s * sin a) * (if y < 0.0 then -1.0 else 1.0),
    0.0,
    s
  ]
