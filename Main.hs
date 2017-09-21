module Main where

import Parser

main :: IO ()
main = do
  seminar  <- leseSeminar "jena/"
  print seminar
