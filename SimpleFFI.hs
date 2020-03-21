{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Foreign
import Foreign.C.Types
import Data.Foldable

-- double sin(double x);
foreign import ccall "math.h sin"
  c_sin :: CDouble -> CDouble

fastsin :: Double -> Double
fastsin x = realToFrac (c_sin (realToFrac x))

main :: IO ()
main = do
  x <- pure $ fmap fastsin [1,2 .. 10000]
  print ()
