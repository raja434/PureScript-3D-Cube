module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (noFlags)

string = "position: relative; transform-style: preserve-3d; transform: matrix3d(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1) rotate3d(0, 0, 0, 0deg);"

main :: forall e. Eff (console :: CONSOLE| e) Unit
main = do
  let res = regex string noFlags
  log "You should add some tests."
