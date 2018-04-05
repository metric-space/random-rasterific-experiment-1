module Main where

import Lib

import Codec.Picture
import Graphics.Rasterific
import Graphics.Rasterific.Texture


fileNames :: [String]
fileNames = ["gintoki.png", "gintoki2.png"]


upperCorner :: Point  -- Point is a type synonym for V2 Float
upperCorner = V2 0 0


setAlphaChannel :: Integer -> Image PixelRGBA8 -> Image PixelRGBA8
setAlphaChannel alpha = pixelMap $ \(PixelRGBA8 x y z _) -> PixelRGBA8 x y z (fromInteger alpha)


getPictureFiles :: [String] -> IO (Either String [DynamicImage])
getPictureFiles fileNames = (sequenceA . (map readImage) $ fileNames) >>= (return . sequenceA)
 

makeSurePngFile :: DynamicImage -> Either String (Image PixelRGBA8)
makeSurePngFile imageFile = case imageFile of 
                              (ImageRGBA8 x) -> Right x 
                              _ -> Left "Not a png file ... "


makeSurePngFiles :: [DynamicImage] -> Either String [Image PixelRGBA8]
makeSurePngFiles = sequenceA . (map makeSurePngFile)


roughSketch :: [Image PixelRGBA8] -> Image PixelRGBA8
roughSketch x = let white = PixelRGBA8 255 255 255 255 
                    (mainPicture:overlay:_) = x 
                    updatedTexture = setAlphaChannel 50 overlay
                in renderDrawing 600 600 white (drawImageAtSize mainPicture 0 upperCorner 600 600 
                                              >> drawImageAtSize updatedTexture 0 upperCorner 600 600)
                 
main :: IO ()
main = do
  output <- (getPictureFiles fileNames)
  case (output >>= makeSurePngFiles) of 
    Left msg -> putStrLn . ("Error "++) $ msg 
    Right x -> (writePng "yourimage.png" . roughSketch) $ x

