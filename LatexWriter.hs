{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module LatexWriter where

import Text.LaTeX
import Stundenplan
import Control.Monad
import Text.LaTeX.Base.Class

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
  --let stringList = map (\ze ->mconcat [textbf (zeit ze), newline])  $ zeiteinheiten (seminar globalerStundenplan)
  --let themen = map schreibeThemenZuZeiteinheit $ zeiteinheiten (seminar globalerStundenplan)
  --fromString $ mconcat stringList
  
  mconcat [textbf "New",newline,  "Test"]
  

schreibeThemenZuZeiteinheit zeiteinheit=
 fromString "Themen"
  
  
 
  


