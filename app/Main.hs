{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Regex

main :: IO ()
main = do
  let Right r = Regex.compile "[^a-zA-Z]*([a-zA-Z]*)" []
  print $ Regex.match r "123hola1234" []
