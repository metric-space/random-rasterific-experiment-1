module Main where

import System.Environment


import Lib


errorMessage :: String
errorMessage = "Error: 2 args required:  mainFileName outFileName "


main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then putStrLn errorMessage
    else realMain args

