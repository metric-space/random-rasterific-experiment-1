module Lib
    ( realMain
    ) where

import Control.Monad ((<=<))
import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture


fileNames :: [String]
fileNames = ["gintoki.jpg", "gintoki2.jpg"]


upperCorner :: Point  -- Point is a type synonym for V2 Float
upperCorner = V2 0 0


whiteBG ::  PixelRGBA8
whiteBG = PixelRGBA8 255 255 255 255


myDrawImage :: Image a -> Float -> Float -> Drawing a ()
myDrawImage image w h = drawImageAtSize image 0 upperCorner w h


setAlphaChannel :: Integer -> Image PixelRGBA8 -> Image PixelRGBA8
setAlphaChannel alpha = pixelMap $ \(PixelRGBA8 x y z _) -> PixelRGBA8 x y z (fromInteger alpha)


getPictureFiles :: [String] -> IO (Either String [DynamicImage])
getPictureFiles =  return . sequenceA <=< (traverse readImage)  
 

makeSurePngFiles :: [DynamicImage] -> [Image PixelRGBA8]
makeSurePngFiles = fmap convertRGBA8


roughSketch :: [Image PixelRGBA8] -> Image PixelRGBA8
roughSketch x = let (mainPicture:overlay:_) = x 
                    updatedTexture = setAlphaChannel 128 overlay
                    Image w h _  = mainPicture
                    mw:mh:_      = fmap toEnum [w,h]
                in renderDrawing w h whiteBG $ (myDrawImage mainPicture mw mh 
                                               >> myDrawImage updatedTexture mw mh)

realMain :: [String] -> IO ()
realMain args = do 
  output <- getPictureFiles . init $ args 
  case (makeSurePngFiles <$> output) of 
    Left msg -> putStrLn . ("Error "++) $ msg 
    Right x -> (writePng (last args) . roughSketch) $ x
