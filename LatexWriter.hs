{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module LatexWriter where

import Data.List
import Control.Monad
import Data.List.Split
import Data.Time (fromGregorian) 
import Data.Time.Calendar.WeekDate (toWeekDate)
import GHC.Exts (sortWith, groupWith)
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
       
zeitspanneZuZeitstring :: Zeitspanne -> String
zeitspanneZuZeitstring (Zeitspanne _ b Nothing) = b
zeitspanneZuZeitstring (Zeitspanne _ b (Just e)) = b ++ " - " ++ e
  
datumZuTag :: String -> String
datumZuTag datum =
  let split = map (fromInteger.read) $ splitOn "-" datum
      (_,_,wtag) = toWeekDate  $ fromGregorian (toInteger (split !! 0)) (split !! 1) (split !! 2)
  in ["Montag","Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"] !!  (wtag-1)


-- By executing 'execLaTeXT' you run the 'LaTeXT' monad and make a 'LaTeX' value as output.
-- With 'renderFile' you render it to 'Text' and write it in a file.
makeLatex :: LokalStundenplan -> IO ()
makeLatex plan = do
  info <- readFile "extern/info.tex"
  execLaTeXT (addPreamble (schreibeGlobalenPlan plan)) >>= renderFile "out/GlobalerStundenplan.tex"
  execLaTeXT (addPreamble (schreibeLokalenPlan plan info)) >>= renderFile "out/LokalerStundenplan.tex"
  
addPreamble :: LaTeXT_ IO -> LaTeXT_ IO
addPreamble content=do
    thePreamble
    document content


-- Preamble with some basic info.
thePreamble :: LaTeXT_ IO
thePreamble = do
  documentclass [a4paper] article
  usepackage [] "fullpage"
  usepackage ["german", "ngerman"] "babel"
  usepackage ["utf8"] "inputenc"
  

schreibeGlobalenPlan :: LokalStundenplan -> LaTeXT_ IO 
schreibeGlobalenPlan (LokalStundenplan globalerStundenplan lokalBelegungen) = do
  let physikeinheiten = einheiten (seminar globalerStundenplan)
  let stringList = map zeiteinheitToTex  physikeinheiten
  let ls =LokalStundenplan globalerStundenplan lokalBelegungen
  let themen = map (schreibeThemenZuZeiteinheit ls) physikeinheiten
  let glob=concat $ transpose [stringList,themen]
  mconcat glob

zeiteinheitToTex :: Zeiteinheit -> LaTeXT_ IO
zeiteinheitToTex ze = mconcat [(center.large.textbf.fromString.show.zeiteinheitZuZeitspanne) ze]

schreibeThemenZuZeiteinheit :: LokalStundenplan -> Zeiteinheit -> LaTeXT_ IO
schreibeThemenZuZeiteinheit (LokalStundenplan globalerStundenplan lokalBelegungen) zeiteinheit=do
  let ls =LokalStundenplan globalerStundenplan lokalBelegungen
  let content= mconcat $ map (\(t,b,r)-> mconcat [(fromString t)&(fromString b)&(fromString r)&(fromString (show (findeTeilnehmerzahl ls zeiteinheit (findeThemaByName (seminar globalerStundenplan) t)))) ,lnbk,hline])   (findeThemenBetreuerRaueme globalerStundenplan zeiteinheit)
  let tab =mconcat [raw "\\begin{tabular} {|p{5cm}|p{4cm}|p{4cm}|p{2cm}}",
                    hline,
                    ((textbf "Thema") & (textbf "Betreuer") & (textbf "Raum")& (textbf "Anzahl")),
                    lnbk,
                    hline,
                    content,
                    raw "\\end{tabular}"] 
  mconcat [center tab]
 
 
findeThemaByName :: Seminar -> String -> Thema
findeThemaByName seminar name =
  let filt = filter (\t -> (titel (tnode t))==name) (themen seminar)
  in if (length filt)==1 then head filt else error ("Fehler bei "++name)
  
findeThemenBetreuerRaueme::GlobalStundenplan -> Zeiteinheit -> [(String,String,String)]
findeThemenBetreuerRaueme plan zeiteinheit= map (\t -> (titel (tnode t), 
                                                        personZuName (bPerson (findeBetreuer plan zeiteinheit t)),
                                                        titel (rnode (findeRaum plan zeiteinheit t))))
                                                $ findeThemen plan zeiteinheit
                                                          
personZuName :: Person -> String
personZuName (Person _ v n _) = v ++ " "++ n                                                          
                                                         
findeThemen :: GlobalStundenplan -> Zeiteinheit -> [Thema]
findeThemen globalerStundenplan zeiteinheit= [thema | GlobalBelegung thema zeiteinheit'<-sortiereGlobaleBelegungen globalerStundenplan, zeiteinheit==zeiteinheit']

findeRaum :: GlobalStundenplan -> Zeiteinheit -> Thema -> Raum
findeRaum globalerStundenplan zeiteinheit thema = rRaum $ head $ filter (\rb -> (rGlobalBelegung rb)==GlobalBelegung thema zeiteinheit) $ raumBelegungen globalerStundenplan

findeBetreuer :: GlobalStundenplan -> Zeiteinheit -> Thema -> BetreuerIn
findeBetreuer globalerStundenplan zeiteinheit thema = bBetreuerIn $ head $ filter (\bb -> (bGlobalBelegung bb)==GlobalBelegung thema zeiteinheit ) $ betreuerBelegungen globalerStundenplan


sortiereGlobaleBelegungen :: GlobalStundenplan -> [GlobalBelegung]
sortiereGlobaleBelegungen plan= sortWith (zeit.gbZeiteinheit)  (globalBelegungen plan)

sortiereRaumBelegungen :: GlobalStundenplan -> [RaumBelegung]
sortiereRaumBelegungen plan= sortWith (zeit.gbZeiteinheit.rGlobalBelegung)  (raumBelegungen  plan)

sortiereBetreuerBelegungen :: GlobalStundenplan -> [BetreuerBelegung]
sortiereBetreuerBelegungen plan= sortWith (zeit.gbZeiteinheit.bGlobalBelegung)  (betreuerBelegungen  plan)

schreibeLokalenPlan :: LokalStundenplan -> String -> LaTeXT_ IO
schreibeLokalenPlan lokalerStundenplan info = do
  let schuelerplan = map (schreibePlanSchueler lokalerStundenplan info) $ (schuelerInnen.seminar.globalStundenplan) lokalerStundenplan
  let betreuerplan = map (schreibePlanBetreuer lokalerStundenplan info) $ (betreuerInnen.seminar.globalStundenplan) lokalerStundenplan
  mconcat (schuelerplan++betreuerplan)

personToTex :: Person -> LaTeXT_ IO 
personToTex p = (center.large.textbf.fromString) ("Stundenplan von "++(vorname p)++" "++(nachname p))

schreibePlanSchueler :: LokalStundenplan -> String -> SchuelerIn -> LaTeXT_ IO  
schreibePlanSchueler lokalerStundenplan info schueler= do
  let tage = groupWith (ztag.zeiteinheitZuZeitspanne) $ zeiteinheiten $ seminar $ globalStundenplan $ lokalerStundenplan
  mconcat $
    personToTex (sPerson schueler) :
    map (schreibeTag lokalerStundenplan schueler) tage ++
    [raw (fromString info), newpage]
    
schreibeTag :: LokalStundenplan -> SchuelerIn -> [Zeiteinheit] -> LaTeXT_ IO  
schreibeTag lokalerStundenplan schueler einheiten=do
  let uberschrift = (large.textbf.fromString.ztag.zeiteinheitZuZeitspanne.head) einheiten
  let tab = mconcat $ [raw "\\begin{tabular} {|p{3cm} p{6cm} p{6cm}| }",
                       hline] ++
                      (map (schreibeThema lokalerStundenplan schueler) einheiten) ++
                      [raw "\\end{tabular}"]
  mconcat [uberschrift,lnbk,tab,vspace (Mm 5),lnbk]
  
schreibeThema :: LokalStundenplan -> SchuelerIn -> Zeiteinheit -> LaTeXT_ IO
schreibeThema (LokalStundenplan globalerPlan lokaleBelegungen) schueler zeiteinheit=
  case (zTyp zeiteinheit) of
    Physikeinheit -> schreibePhysikeinheit (LokalStundenplan globalerPlan lokaleBelegungen) schueler zeiteinheit
    Exkursion -> schreibeExkursion (LokalStundenplan globalerPlan lokaleBelegungen) schueler
    Anderes ->  let zeitstring = zeitspanneZuZeitstring (zeiteinheitZuZeitspanne zeiteinheit) 
                in mconcat [((textbf.fromString) zeitstring)&((textbf.fromString.titel.znode) zeiteinheit)&((textbf.fromString.ort) zeiteinheit),lnbk,hline]
      
schreibePhysikeinheit :: LokalStundenplan -> SchuelerIn -> Zeiteinheit -> LaTeXT_ IO
schreibePhysikeinheit (LokalStundenplan globalerPlan lokaleBelegungen) schueler zeiteinheit=do
  let lokaleBelegung' = filter (\lb-> (lSchuelerIn lb)==schueler && ((gbZeiteinheit.lGlobalBelegung) lb)==zeiteinheit) lokaleBelegungen
  if (length lokaleBelegung') ==0 
    then mconcat["frei"&""&"",lnbk,hline]
    else schreibePhysikeinheit' globalerPlan zeiteinheit lokaleBelegung' (LokalStundenplan globalerPlan lokaleBelegungen)
      
schreibePhysikeinheit' :: GlobalStundenplan -> Zeiteinheit -> [LokalBelegung] -> LokalStundenplan -> LaTeXT_ IO      
schreibePhysikeinheit' globalerPlan zeiteinheit lokaleBelegung lokalStundenplan=do
  let thema = gbThema $ lGlobalBelegung $ head lokaleBelegung
  let betreuer = findeBetreuer globalerPlan zeiteinheit thema
  let raum = findeRaum globalerPlan zeiteinheit thema
  let zeitstring = zeitspanneZuZeitstring (zeiteinheitZuZeitspanne zeiteinheit)
  mconcat[((textbf.fromString) zeitstring) & ((textbf.fromString.titel.tnode) thema) & ((textbf.fromString.titel.rnode) raum),
          lnbk, 
          ""& (fromString ( "Betreuer: "++ ((personZuName.bPerson) betreuer))) & ""
          ,lnbk,
          ""& (fromString ("ca. " ++ (show (findeTeilnehmerzahl lokalStundenplan zeiteinheit thema))++" Teilnehmer")) & "" 
          , lnbk, hline]
          
findeTeilnehmerzahl :: LokalStundenplan -> Zeiteinheit -> Thema -> Int
findeTeilnehmerzahl (LokalStundenplan globalerPlan lokaleBelegungen) zeiteinheit thema =
  length $ filter (\(LokalBelegung (GlobalBelegung t z) _) -> (t==thema)&&(z==zeiteinheit)) lokaleBelegungen
          
schreibeExkursion :: LokalStundenplan -> SchuelerIn -> LaTeXT_ IO
schreibeExkursion (LokalStundenplan globalerPlan lokaleBelegung) schueler = do
  let zeiteinheit =exkursionseinheit (seminar globalerPlan)
  let thema = head [t| (LokalBelegung (GlobalBelegung t z) s) <- lokaleBelegung, (z==zeiteinheit)&&(s==schueler)]
  let betreuer = findeBetreuer globalerPlan zeiteinheit thema
  let zeitstring = zeitspanneZuZeitstring (zeiteinheitZuZeitspanne zeiteinheit)
  mconcat[((textbf.fromString) zeitstring) & ((textbf.fromString.titel.tnode) thema) & "",
          lnbk, 
          ""& (fromString ( "Betreuer: "++ ((personZuName.bPerson) betreuer))) & "" 
          , lnbk, hline]
          
          
schreibePlanBetreuer :: LokalStundenplan -> String -> BetreuerIn -> LaTeXT_ IO  
schreibePlanBetreuer lokalerStundenplan info betreuer= do
  let tage = groupWith (ztag.zeiteinheitZuZeitspanne) $ zeiteinheiten $ seminar $ globalStundenplan $ lokalerStundenplan
  mconcat $
    personToTex (bPerson betreuer) :
    map (schreibeTagBetreuer lokalerStundenplan betreuer) tage ++
    [ raw (fromString info), newpage]
    
schreibeTagBetreuer :: LokalStundenplan -> BetreuerIn -> [Zeiteinheit] -> LaTeXT_ IO  
schreibeTagBetreuer lokalerStundenplan betreuer einheiten=do
  let uberschrift = (large.textbf.fromString.ztag.zeiteinheitZuZeitspanne.head) einheiten
  let tab = mconcat $ [raw "\\begin{tabular} {|p{3cm} p{6cm} p{6cm}| }",
                       hline] ++
                      (map (schreibeThemaBetreuer lokalerStundenplan betreuer) einheiten) ++
                      [raw "\\end{tabular}"]
  mconcat [uberschrift,lnbk,tab,vspace (Mm 5),lnbk]
    
schreibeThemaBetreuer :: LokalStundenplan -> BetreuerIn -> Zeiteinheit -> LaTeXT_ IO
schreibeThemaBetreuer (LokalStundenplan globalerPlan lokaleBelegungen) betreuer zeiteinheit=
  case (zTyp zeiteinheit) of
    Physikeinheit -> schreibePhysikeinheitBetreuer (LokalStundenplan globalerPlan lokaleBelegungen) betreuer zeiteinheit
    Exkursion -> schreibeExkursionBetreuer (LokalStundenplan globalerPlan lokaleBelegungen) betreuer
    Anderes ->  let zeitstring = zeitspanneZuZeitstring (zeiteinheitZuZeitspanne zeiteinheit) 
                in mconcat [((textbf.fromString) zeitstring)&((textbf.fromString.titel.znode) zeiteinheit)&((textbf.fromString.ort) zeiteinheit),lnbk,hline]
                
schreibeExkursionBetreuer :: LokalStundenplan -> BetreuerIn -> LaTeXT_ IO
schreibeExkursionBetreuer (LokalStundenplan globalerPlan lokaleBelegung) betreuer = do
  let zeiteinheit =exkursionseinheit (seminar globalerPlan)
  let themen = [t | (BetreuerBelegung (GlobalBelegung t z) b) <- (betreuerBelegungen globalerPlan) , (z==zeiteinheit) && (b==betreuer)] 
  let zeitstring = zeitspanneZuZeitstring (zeiteinheitZuZeitspanne zeiteinheit)
  if (length themen) == 1
    then  
      let teilnehmerzahl = findeTeilnehmerzahl (LokalStundenplan globalerPlan lokaleBelegung) zeiteinheit (head themen)
      in mconcat[((textbf.fromString) zeitstring) & ((textbf.fromString.titel.tnode) (head themen)) & "",
          lnbk,
          "" & fromString ("ca. "++ (show teilnehmerzahl)++" Teilnehmer") & ""
          ,lnbk,hline]
    else mconcat[""&"Such dir was aus (nicht FRM2)"&"",lnbk,hline]
    
schreibePhysikeinheitBetreuer :: LokalStundenplan -> BetreuerIn -> Zeiteinheit -> LaTeXT_ IO
schreibePhysikeinheitBetreuer (LokalStundenplan globalerPlan lokaleBelegungen) betreuer zeiteinheit = do
  let themen = [t | (BetreuerBelegung (GlobalBelegung t z) b) <- (betreuerBelegungen globalerPlan) , (z==zeiteinheit) && (b==betreuer)] 
  let zeitstring = zeitspanneZuZeitstring (zeiteinheitZuZeitspanne zeiteinheit)
  if (length themen) == 1
    then 
      let teilnehmerzahl = findeTeilnehmerzahl (LokalStundenplan globalerPlan lokaleBelegungen) zeiteinheit (head themen)
      in mconcat[((textbf.fromString) zeitstring) & ((textbf.fromString.titel.tnode) (head themen)) & "",
          lnbk,
          "" & fromString ("ca. "++ (show teilnehmerzahl)++" Teilnehmer") & ""
          ,lnbk, hline]
    else mconcat[((textbf.fromString) zeitstring)&"frei"&"",lnbk,hline] 
