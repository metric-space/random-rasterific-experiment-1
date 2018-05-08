module Lib
    ( realMain
    ) where

import Control.Monad ((<=<))
import Data.List
import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture


whiteBG ::  PixelRGB8
whiteBG = PixelRGB8 0 0 0


filterImage :: Image PixelRGB8 -> Image PixelRGB8
filterImage = pixelMap filterImage_

filterImage_ :: PixelRGB8 -> PixelRGB8
filterImage_ pixel@(PixelRGB8 x y z) = if (length . nub $ [x,y,z]) == 1
                                         then whiteBG
                                         else pixel


realMain :: [String] -> IO ()
realMain args = do
  output <- readImage . head $ args
  case output of
    Left msg -> putStrLn . ("Error "++) $ msg
    Right x -> (writePng (last args) . filterImage. convertRGB8) x
