module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (addClass, body, create, css, getCss, getPageX, getPageY, off, on, select, setAttr)
import Control.Monad.Eff.JQuery (append) as JQ
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)
import Math (sqrt)
import Matrices (RotationVector, TransformMatrix, multiply, noRotation, noTransformation, angle, rotationVector, toString, toTransformMatrix)


framesPerSecond :: Number
framesPerSecond = 60.0


drawCube :: forall e. Eff (dom :: DOM |e) Unit
drawCube = do
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

  css { width: "100%", height: "100%" } body

  face <- select ".face"
  css {
  	position : "absolute",
  	width : "200px",
  	height : "200px",
    border : "solid green 3px",
    backgroundColor : "rgba(0, 192, 255, 0.17)"
  } face


rotateCube :: forall h e. STRef h TransformMatrix
  -> RotationVector
  -> Eff (dom :: DOM, st:: ST h |e) Unit
rotateCube transformRef rotation = do
  cube <- select ".cube"
  transform <- readSTRef transformRef
  css {
    transform: "matrix3d" <> toString transform
                  <> " rotate3d" <> (toString $ multiply transform rotation)
  } cube


startMouseHandlers :: forall h e. STRef h TransformMatrix
  -> STRef h RotationVector
  -> Eff (dom :: DOM, st :: ST h | e) Unit
startMouseHandlers transformRef velocityRef = do
  body <- body
  let downHandler event jq = do
        downX <- getPageX event
        downY <- getPageY event
        let moveHandler event' jq' = do
              x <- getPageX event'
              y <- getPageY event'
              let dx = negate (y - downY)
              let dy = x - downX
              let rotation = rotationVector [dx, dy, 0.0,
                    sqrt (dx * dx + dy * dy)]
              rotateCube transformRef rotation
        let upHandler event' jq' = do
              cube <- select ".cube"
              off "mousemove" body
              t <- getCss "transform" cube
              writeSTRef transformRef (toTransformMatrix t)
        on "mousemove" moveHandler body
        on "mouseup" upHandler body
  on "mousedown" downHandler body


startSpinner :: forall h e. STRef h TransformMatrix
  -> STRef h RotationVector
  -> Eff (dom :: DOM, st :: ST h | e) Unit
startSpinner transformRef velocityRef = do
  let spinner = do
        rotation <- readSTRef velocityRef
        if angle rotation /= 0.0
          then do
            rotateCube transformRef rotation
            cube <- select ".cube"
            t <- getCss "transform" cube
            void $ writeSTRef transformRef (toTransformMatrix t)
            w <- window
            void $ requestAnimationFrame spinner w
          else pure unit
  spinner


run :: forall e h. Eff (dom :: DOM, st :: ST h | e) Unit
run = do
  transformRef <- newSTRef noTransformation
  velocityRef <- newSTRef $ noRotation
  startSpinner transformRef velocityRef
  startMouseHandlers transformRef velocityRef
  pure unit


main :: forall h e. Eff (dom :: DOM, st :: ST h | e) Unit
main = do
  drawCube
  run
