module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (addClass, body, create, css, select, setAttr)
import Control.Monad.Eff.JQuery (append) as JQ
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

  css {
  	transform : "translateX(-100px) translateY(-100px) translateZ(100px)"
  } frontFace

  css {
  	transform : "translateX(-100px) translateY(-100px) translateZ(-100px)"
  } backFace
  css {
  	transform : "translateY(-100px) rotateY(90deg)"
  } rightFace
  css {
    transform : "translateY(-100px) translateX(-200px) rotateY(90deg)"
  } leftFace
  css {
    transform : "translateX(-100px) translateY(-200px) rotateX(90deg)"
  } topFace
  css {
    transform : "translateX(-100px) rotateX(90deg)"
  } bottomFace

  css {
    position: "relative",
  	transformStyle: "preserve-3d"
  } cube

  JQ.append frontFace cube
  JQ.append backFace cube
  JQ.append rightFace cube
  JQ.append leftFace cube
  JQ.append topFace cube
  JQ.append bottomFace cube

  cubeWrapper <- create "<div>"
  setAttr "id" "cube-wrapper" cubeWrapper
  css {
    position : "absolute",
    left : "50%",
    top : "50%",
    perspective: "1500px"
  } cubeWrapper
  JQ.append cube cubeWrapper

  body <- body
  JQ.append cubeWrapper body

  face <- select ".face"
  css {
  	position : "absolute",
  	width : "200px",
  	height : "200px",
    border : "solid green 3px",
    backgroundColor : "rgba(0, 192, 255, 0.17)"
  } face


main :: forall e. Eff (dom :: DOM | e) Unit
main = generateCube
