module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (addClass, append, body, create, setAttr)
import DOM (DOM)

generateCube :: forall e. Eff (dom :: DOM |e) Unit
generateCube = do
  frontFace <- create "<div>"
  setAttr "id" "front_face" frontFace
  addClass "face" frontFace

  backFace <- create "<div>"
  setAttr "id" "back_face" backFace
  addClass "face" backFace

  rightFace <- create "<div>"
  setAttr "id" "right_face" rightFace
  addClass "face" rightFace

  leftFace <- create "<div>"
  setAttr "id" "left_face" leftFace
  addClass "face" leftFace

  topFace <- create "<div>"
  setAttr "id" "top_face" topFace
  addClass "face" topFace

  bottomFace <- create "<div>"
  setAttr "id" "bottom_face" bottomFace
  addClass "face" bottomFace

  cube <- create "<div>"
  addClass "cube" cube

  append frontFace cube
  append backFace cube
  append rightFace cube
  append leftFace cube
  append topFace cube
  append bottomFace cube

  cubeWrapper <- create "<div>"
  setAttr "id" "cube-wrapper" cubeWrapper
  append cube cubeWrapper

  body <- body
  append cubeWrapper body


main :: forall e. Eff (dom :: DOM | e) Unit
main = generateCube
