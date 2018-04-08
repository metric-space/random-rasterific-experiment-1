module Main where

import System.Environment


import Lib


errorMessage :: String
errorMessage = "Error: 3 args required:  mainFileName layerImageFileName outFileName "


main :: IO ()
main = do
  args <- getArgs
  if length args /= 3
    then putStrLn errorMessage
    else realMain args

