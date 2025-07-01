module Main (main) where

import Haskeline.Bluefin.Dynamic qualified as BluefinD
import Haskeline.Bluefin.Static qualified as BluefinS
import Haskeline.Class qualified as Class
import Haskeline.Effectful.Dynamic qualified as EffectfulD
import Haskeline.Effectful.Static qualified as EffectfulS

main :: IO ()
main = do
  Class.runIO
  EffectfulS.runIO
  EffectfulD.runIO
  BluefinS.runIO
  BluefinD.runIO
