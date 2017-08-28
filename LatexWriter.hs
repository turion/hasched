{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module LatexWriter where

import Data.List
import Control.Monad
import Text.LaTeX
import Text.LaTeX.Base.Class
import Stundenplan

-- By executing 'execLaTeXT' you run the 'LaTeXT' monad and make a 'LaTeX' value as output.
-- With 'renderFile' you render it to 'Text' and write it in a file.
makeLatex :: LokalStundenplan -> IO ()
makeLatex plan = do
  execLaTeXT (addPreamble (schreibeGlobalenPlan (globalStundenplan plan))) >>= renderFile "out/GlobalerStundenplan.tex"
  

addPreamble content=do
    thePreamble
    document content


-- Preamble with some basic info.
thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
  documentclass [] article


schreibeGlobalenPlan globalerStundenplan = do
  let stringList = map zeiteinheitToTex  $ zeiteinheiten (seminar globalerStundenplan)
  let themen = map schreibeThemenZuZeiteinheit $ zeiteinheiten (seminar globalerStundenplan)
  let glob=concat $ transpose [stringList,themen]
  mconcat glob

zeiteinheitToTex ze = mconcat [textbf ( fromString (zeit ze)), newline]

schreibeThemenZuZeiteinheit zeiteinheit=
 fromString "Themen"
  
  
 
  


