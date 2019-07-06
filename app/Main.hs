-- {-# LANGUAGE OverloadedStrings #-}
module Main where
import Template
import Writer
import GUI


main :: IO ()
main = do
  writeRTF
  -- startGUI
