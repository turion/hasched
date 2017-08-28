{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module LatexWriter where

import Data.List
import Control.Monad
import GHC.Exts (sortWith)
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
  let themen = map (schreibeThemenZuZeiteinheit globalerStundenplan) $ zeiteinheiten (seminar globalerStundenplan)
  let glob=concat $ transpose [stringList,themen]
  mconcat glob

zeiteinheitToTex ze = mconcat [(center.large.textbf.fromString.zeit) ze]


schreibeThemenZuZeiteinheit globalerStundenplan zeiteinheit=do
  let content= mconcat $ map (\(t,b,r)-> mconcat [(fromString t)&(fromString b)&(fromString r),lnbk,hline]) (findeThemenBetreuerRaueme globalerStundenplan zeiteinheit)
  let tab=tabular 
            Nothing  --location center 
            [VerticalLine,CenterColumn, VerticalLine, CenterColumn, VerticalLine, CenterColumn,VerticalLine]
            (mconcat [hline,((textbf "Thema") & (textbf "Betreuer") & (textbf "Raum")),lnbk,  hline, content])
  mconcat [tab]
  
findeThemenBetreuerRaueme::GlobalStundenplan -> Zeiteinheit -> [(String,String,String)]
findeThemenBetreuerRaueme plan zeiteinheit= zip3 (findeThemenString plan zeiteinheit) ["Betr", "Betr"] (findeRaumString plan zeiteinheit)
  
findeThemenString :: GlobalStundenplan -> Zeiteinheit -> [String]
findeThemenString globalerStundenplan zeiteinheit = [ titel (tnode thema) | GlobalBelegung thema zeiteinheit'<-sortiereGlobaleBelegungen globalerStundenplan, zeiteinheit==zeiteinheit']

findeRaumString :: GlobalStundenplan -> Zeiteinheit -> [String]
findeRaumString globalerStundenplan zeiteinheit = [ titel (tnode thema) | GlobalBelegung thema zeiteinheit'<-sortiereGlobaleBelegungen globalerStundenplan, zeiteinheit==zeiteinheit']
  
  
sortiereGlobaleBelegungen :: GlobalStundenplan -> [GlobalBelegung]
sortiereGlobaleBelegungen plan= sortWith (zeit.gbZeiteinheit)  (globalBelegungen plan)

sortiereRaumBelegungen :: GlobalStundenplan -> [RaumBelegung]
sortiereRaumBelegungen plan= sortWith (zeit.gbZeiteinheit.rGlobalBelegung)  (raumBelegungen  plan)
  


