module Matrices where

import Prelude

import Data.Array (length)
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(..), contains, split, stripPrefix, stripSuffix)
import LinearAlgebra.Matrix (Matrix, element, fromArray, identity, rows)
import LinearAlgebra.Matrix (multiply) as M


newtype RotationVector = RotationVector (Matrix Number)

rotationVector :: Array Number -> RotationVector
rotationVector a =
  if length a == 4
  then
    RotationVector (fromMaybe (identity 1) (fromArray 4 1 a))
  else
    noRotation

noRotation :: RotationVector
noRotation = RotationVector (fromMaybe (identity 1)
  (fromArray 4 1 [0.0, 0.0, 0.0, 0.0]))


newtype TransformMatrix = TransformMatrix (Matrix Number)

transformMatrix :: Array Number -> TransformMatrix
transformMatrix a =
  if length a == 16
  then
    TransformMatrix (fromMaybe (identity 1) (fromArray 4 4 a))
  else
    noTransformation

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
toTransformMatrix str =
  case contains (Pattern "matrix3d(") str of
  true -> do
            let a = foldl (\ar s -> ar <> [fromMaybe 0.0 (fromString s)]) [] $
                  split (Pattern ", ") $
                  fromMaybe "" (stripSuffix (Pattern ")") $
                  fromMaybe "" (stripPrefix (Pattern "matrix3d(") str))
            if length a /= 16
              then
                noTransformation
              else
                transformMatrix a
  _ -> noTransformation


multiply :: TransformMatrix -> RotationVector -> RotationVector
multiply (TransformMatrix mt) (RotationVector v) = RotationVector (M.multiply mt v)

angle :: RotationVector -> Number
angle (RotationVector m) = fromMaybe 0.0 $ element 3 0 m
