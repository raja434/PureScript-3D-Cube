module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (addClass, body, create, css, getPageX, getPageY, on, off, select, setAttr)
import Control.Monad.Eff.JQuery (append) as JQ
import Control.Monad.ST (ST, STRef, newSTRef, readSTRef, writeSTRef)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)


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

  face <- select ".face"
  css {
  	position : "absolute",
  	width : "200px",
  	height : "200px",
    border : "solid green 3px",
    backgroundColor : "rgba(0, 192, 255, 0.17)"
  } face


rotateCube :: forall h e. STRef h {x::Number, y::Number}
  -> {x::Number, y::Number}
  -> Eff (dom :: DOM, st :: ST h |e) Unit
rotateCube angleRef newAngle = do
  cube <- select ".cube"
  angle <- readSTRef angleRef
  css {
    transform: "rotateX(" <> show(newAngle.x) <> "deg)"
                  <> " rotateY(" <> show(newAngle.y) <> "deg)"
  } cube
  void $ writeSTRef angleRef newAngle


startMouseHandlers :: forall h e. STRef h {x::Number, y::Number}
  -> STRef h {x::Number, y::Number}
  -> Eff (dom :: DOM, st :: ST h | e) Unit
startMouseHandlers angleRef velocityRef = do
  lastMousePos <- newSTRef {x: 0.0, y: 0.0}
  body <- body
  let moveHandler event jq = do
        x <- getPageX event
        y <- getPageY event
        lastPos <- readSTRef lastMousePos
        angle <- readSTRef angleRef
        let newAngle = {
              x: angle.x + negate (y - lastPos.y),
              y: angle.y + x - lastPos.x
            }
        rotateCube angleRef newAngle
        void $ writeSTRef lastMousePos {x: x, y: y}
  let upHandler event jq = do
        off "mousemove" body
  let downHandler event jq = do
        lastX <- getPageX event
        lastY <- getPageY event
        void $ writeSTRef lastMousePos {x: lastX, y: lastY}
        on "mousemove" moveHandler body
        on "mouseup" upHandler body
  css {
    width: "100%",
    height: "100%"
  } body
  on "mousedown" downHandler body


startSpinner :: forall h e. STRef h {x::Number, y::Number}
  -> STRef h {x::Number, y::Number}
  -> Eff (dom :: DOM, st :: ST h | e) Unit
startSpinner angleRef velocityRef = do
  let spinner = do
        vel <- readSTRef velocityRef
        angle <- readSTRef angleRef
        let
          newAngle = {
            x: angle.x + (vel.x / 60.0),
            y: angle.y + (vel.y / 60.0)
          }
        rotateCube angleRef newAngle
        w <- window
        void $ requestAnimationFrame spinner w
  spinner


run :: forall e h. Eff (dom :: DOM, st :: ST h | e) Unit
run = do
  angleRef <- newSTRef { x: 0.0, y: 0.0 }
  velocityRef <- newSTRef { x: 0.0, y: 0.0 }
  startSpinner angleRef velocityRef
  startMouseHandlers angleRef velocityRef


main :: forall h e. Eff (dom :: DOM, st :: ST h | e) Unit
main = do
  drawCube
  run
