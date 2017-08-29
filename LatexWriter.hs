{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module LatexWriter where

import Data.List
import Control.Monad
import Data.List.Split
import Data.Time (fromGregorian) 
import Data.Time.Calendar.WeekDate (toWeekDate)
import GHC.Exts (sortWith)
import Text.LaTeX
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Render
import Stundenplan

data Zeitspanne=Zeitspanne
  { ztag :: String
  , zbeginn :: String
  , zende :: Maybe String
  }
  
instance Show Zeitspanne where
  show (Zeitspanne t b (Just e)) = t ++ " " ++ b ++ " - " ++ e
  show (Zeitspanne t b Nothing) = t ++ " " ++ b
  
zeiteinheitZuZeitspanne :: Zeiteinheit -> Zeitspanne
zeiteinheitZuZeitspanne ze =
  let zeiten= splitOn "bis" $ zeit ze
      datumString = (splitOn " " (zeiten !! 0)) !! 0
      beginnString = (splitOn " " (zeiten !! 0)) !! 1
  in if (length zeiten)==1
       then Zeitspanne (datumZuTag datumString) (take 5 beginnString) Nothing
       else Zeitspanne (datumZuTag datumString) (take 5 beginnString) (Just (((take 5).last.(splitOn " ")) (zeiten !! 1)))
  
datumZuTag :: String -> String
datumZuTag datum =
  let split = map (fromInteger.read) $ splitOn "-" datum
      (_,_,wtag) = toWeekDate  $ fromGregorian (toInteger (split !! 0)) (split !! 1) (split !! 2)
  in ["Montag","Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"] !!  (wtag-1)


-- By executing 'execLaTeXT' you run the 'LaTeXT' monad and make a 'LaTeX' value as output.
-- With 'renderFile' you render it to 'Text' and write it in a file.
makeLatex :: LokalStundenplan -> IO ()
makeLatex plan = do
  execLaTeXT (addPreamble (schreibeGlobalenPlan (globalStundenplan plan))) >>= renderFile "out/GlobalerStundenplan.tex"
  execLaTeXT (addPreamble (schreibeLokalenPlan plan)) >>= renderFile "out/LokalerStundenplan.tex"
  

addPreamble content=do
    thePreamble
    document content


-- Preamble with some basic info.
thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
  documentclass [a4paper] article
  usepackage [] "fullpage"


schreibeGlobalenPlan globalerStundenplan = do
  let physikeinheiten = filter (\ze->(zTyp ze)==Physikeinheit) $ zeiteinheiten (seminar globalerStundenplan)
  let stringList = map zeiteinheitToTex  physikeinheiten
  let themen = map (schreibeThemenZuZeiteinheit globalerStundenplan) physikeinheiten
  let glob=concat $ transpose [stringList,themen]
  mconcat glob

zeiteinheitToTex ze = mconcat [(center.large.textbf.fromString.show.zeiteinheitZuZeitspanne) ze]


schreibeThemenZuZeiteinheit globalerStundenplan zeiteinheit=do
  let content= mconcat $ map (\(t,b,r)-> mconcat [(fromString t)&(fromString b)&(fromString r),lnbk,hline]) (findeThemenBetreuerRaueme globalerStundenplan zeiteinheit)
  {-let tab=tabular 
            Nothing  --location center 
            [VerticalLine,raw "L{5cm}", VerticalLine, raw "L{5cm}", VerticalLine, raw "L{5cm}",VerticalLine]
            (mconcat [hline,((textbf "Thema") & (textbf "Betreuer") & (textbf "Raum")),lnbk,  hline, content])-}
  let tab =mconcat [raw "\\begin{tabular} {|p{5cm}|p{5cm}|p{5cm}|}",
                    hline,
                    ((textbf "Thema") & (textbf "Betreuer") & (textbf "Raum")),
                    lnbk,
                    hline,
                    content,
                    raw "\\end{tabular}"] 
  mconcat [center tab]
  
findeThemenBetreuerRaueme::GlobalStundenplan -> Zeiteinheit -> [(String,String,String)]
findeThemenBetreuerRaueme plan zeiteinheit= zip3 (findeThemenString plan zeiteinheit) (findeBetreuerString plan zeiteinheit) (findeRaumString plan zeiteinheit)
  
findeThemenString :: GlobalStundenplan -> Zeiteinheit -> [String]
findeThemenString globalerStundenplan zeiteinheit = [ titel (tnode thema) | GlobalBelegung thema zeiteinheit'<-sortiereGlobaleBelegungen globalerStundenplan, zeiteinheit==zeiteinheit']

findeRaumString :: GlobalStundenplan -> Zeiteinheit -> [String]
findeRaumString globalerStundenplan zeiteinheit = [ titel (rnode raum) | RaumBelegung (GlobalBelegung _ zeiteinheit') raum  <-sortiereRaumBelegungen globalerStundenplan, zeiteinheit==zeiteinheit']

findeBetreuerString :: GlobalStundenplan -> Zeiteinheit -> [String]
findeBetreuerString globalerStundenplan zeiteinheit = [ (vorname (bPerson betreuer))++" "++(nachname (bPerson betreuer)) | BetreuerBelegung (GlobalBelegung _ zeiteinheit') betreuer  <-sortiereBetreuerBelegungen globalerStundenplan, zeiteinheit==zeiteinheit']
  
  
sortiereGlobaleBelegungen :: GlobalStundenplan -> [GlobalBelegung]
sortiereGlobaleBelegungen plan= sortWith (zeit.gbZeiteinheit)  (globalBelegungen plan)

sortiereRaumBelegungen :: GlobalStundenplan -> [RaumBelegung]
sortiereRaumBelegungen plan= sortWith (zeit.gbZeiteinheit.rGlobalBelegung)  (raumBelegungen  plan)

sortiereBetreuerBelegungen :: GlobalStundenplan -> [BetreuerBelegung]
sortiereBetreuerBelegungen plan= sortWith (zeit.gbZeiteinheit.bGlobalBelegung)  (betreuerBelegungen  plan)


schreibeLokalenPlan lokalerStundenplan = do
  let schuelerplan = map (schreibePlanSchueler lokalerStundenplan) $ (schuelerInnen.seminar.globalStundenplan) lokalerStundenplan
  mconcat schuelerplan
 
personToTex p = (center.large.textbf.fromString) ("Stundenplan von "++(vorname p)++" "++(nachname p))
  
schreibePlanSchueler lokalerStundenplan schueler=
  mconcat [personToTex (sPerson schueler) ,newpage]
  


