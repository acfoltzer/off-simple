module Main where

import Graphics.Formats.OFF.Simple

import Control.Arrow

import Data.Vector ( Vector )
import qualified Data.Vector as V

import Test.HUnit ( assertFailure, (@?=) )
import Test.Framework
import Test.Framework.Providers.HUnit

import System.FilePath

import TestData

samplePrefix = "samples"

fileMatches :: FilePath -> OFF -> Test
fileMatches file off = testCase file $ do
  eoff <- readOFFFile (samplePrefix </> file)
  case eoff of
    Left err -> assertFailure (show err)
    Right off' -> off' @?= off

makeOFF :: ([(Double, Double, Double)], [[Int]]) -> OFF
makeOFF (vs, fs) = 
  OFF { vertices = V.fromList vs
      , faces = V.map (flip Face Nothing) $ V.fromList (map V.fromList fs)
      }

makeOFFC :: ( [(Double, Double, Double)] 
            , [([Int], (Double, Double, Double))])
         -> OFF
makeOFFC (vs, fs) =
  OFF { vertices = V.fromList vs
      , faces = V.map (uncurry Face) $ 
                  V.fromList (map (first V.fromList . second Just) fs)
      }

tests = [ "cube.off" `fileMatches` makeOFF cubeData
        , "cube.offc" `fileMatches` makeOFFC cubecData
        , "dodec.off" `fileMatches` makeOFF dodecData
        , "mushroom.off" `fileMatches` makeOFFC mushroomData
        , "tetra.off" `fileMatches` makeOFF tetraData
        , "tetra.offc" `fileMatches` makeOFFC tetracData
        ]

main :: IO ()
main = defaultMain tests