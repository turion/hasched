{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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

data Zeitspanne = Zeitspanne
  { zTag    :: String
  , zBeginn :: String
  , zEnde   :: Maybe String
  }

instance Show Zeitspanne where
  show (Zeitspanne t b (Just e)) = t ++ " " ++ b ++ " - " ++ e
  show (Zeitspanne t b Nothing)  = t ++ " " ++ b

zeiteinheitZuZeitspanne :: Zeiteinheit -> Zeitspanne
zeiteinheitZuZeitspanne ze =
  let von : rest = splitOn "bis" $ zeit ze
      datumString : beginnString : _ = splitOn " " von
      tag = datumZuTag datumString
      beginnKurz = take 5 beginnString
  in case rest of
    []      -> Zeitspanne tag beginnKurz Nothing
    bis : _ -> Zeitspanne tag beginnKurz $ Just $ take 5 $ last $ splitOn " " bis

datumZuTag :: String -> String
datumZuTag datum =
  let [jahr, monat, tag] = map (fromInteger . read) $ splitOn "-" datum
      (_, _, wtag)       = toWeekDate $ fromGregorian (toInteger jahr) monat tag
  in ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"] !! (wtag-1)


-- By executing 'execLaTeXT' you run the 'LaTeXT' monad and make a 'LaTeX' value as output.
-- With 'renderFile' you render it to 'Text' and write it in a file.
makeLatex :: LokalStundenplan -> IO ()
makeLatex plan = do
  execLaTeXT (addPreamble (schreibeGlobalenPlan (globalStundenplan plan))) >>= renderFile "out/GlobalerStundenplan.tex"
  execLaTeXT (addPreamble (schreibeLokalenPlan plan)) >>= renderFile "out/LokalerStundenplan.tex"

addPreamble :: LaTeXT_ IO -> LaTeXT_ IO
addPreamble content = do
    thePreamble
    document content


-- Preamble with some basic info.
thePreamble :: LaTeXT_ IO
thePreamble = do
  documentclass [a4paper] article
  usepackage [] "fullpage"

schreibeGlobalenPlan :: GlobalStundenplan -> LaTeXT_ IO
schreibeGlobalenPlan globalerStundenplan = do
  let physikeinheiten = filter (\ze -> zTyp ze == Physikeinheit) $ zeiteinheiten $ seminar globalerStundenplan
  let stringList = map zeiteinheitToTex physikeinheiten
  let themen = map (schreibeThemenZuZeiteinheit globalerStundenplan) physikeinheiten
  let glob = concat $ transpose [stringList, themen]
  mconcat glob

zeiteinheitToTex :: Zeiteinheit -> LaTeXT_ IO
zeiteinheitToTex ze = mconcat [center $ large $ textbf $fromString $ show $ zeiteinheitZuZeitspanne ze]

schreibeThemenZuZeiteinheit :: GlobalStundenplan -> Zeiteinheit -> LaTeXT_ IO
schreibeThemenZuZeiteinheit globalerStundenplan zeiteinheit = do
  let content = mconcat $ map (\(t, b, r) -> mconcat [fromString t & fromString b & fromString r, lnbk, hline]) $ findeThemenBetreuerRaueme globalerStundenplan zeiteinheit
      tab     = mconcat [ raw "\\begin{tabular} {|p{5cm}|p{5cm}|p{5cm}|}"
                        , hline
                        , ((textbf "Thema") & (textbf "Betreuer") & (textbf "Raum"))
                        , lnbk
                        , hline
                        , content
                        , raw "\\end{tabular}"
                        ]
  mconcat [center tab]



findeThemenBetreuerRaueme::GlobalStundenplan -> Zeiteinheit -> [(String, String, String)]
findeThemenBetreuerRaueme plan zeiteinheit = map (\t -> ( titel $ tnode t
                                                        , personZuName $ bPerson $ findeBetreuer plan zeiteinheit t
                                                        , titel $ rnode $ findeRaum plan zeiteinheit t
                                                        )
                                                 )
                                                 $ findeThemen plan zeiteinheit

personZuName :: Person -> String
personZuName (Person {..}) = vorname ++ " " ++ nachname

findeThemen :: GlobalStundenplan -> Zeiteinheit -> [Thema]
findeThemen globalerStundenplan zeiteinheit = [thema | GlobalBelegung thema zeiteinheit' <- sortiereGlobaleBelegungen globalerStundenplan, zeiteinheit == zeiteinheit']

-- TODO Unsicher, besser mit Maybe?
findeRaum :: GlobalStundenplan -> Zeiteinheit -> Thema -> Raum
findeRaum globalerStundenplan zeiteinheit thema = rRaum $ head $ filter (\rb -> rGlobalBelegung rb == GlobalBelegung thema zeiteinheit) $ raumBelegungen globalerStundenplan

-- TODO Unsicher, besser mit Maybe?
findeBetreuer :: GlobalStundenplan -> Zeiteinheit -> Thema -> BetreuerIn
findeBetreuer globalerStundenplan zeiteinheit thema = bBetreuerIn $ head $ filter (\bb -> bGlobalBelegung bb == GlobalBelegung thema zeiteinheit) $ betreuerBelegungen globalerStundenplan

-- TODO Besser mit geeigneter Ord-Instanz
sortiereGlobaleBelegungen :: GlobalStundenplan -> [GlobalBelegung]
sortiereGlobaleBelegungen plan = sortWith (zeit . gbZeiteinheit) $ globalBelegungen plan

sortiereRaumBelegungen :: GlobalStundenplan -> [RaumBelegung]
sortiereRaumBelegungen plan = sortWith (zeit . gbZeiteinheit . rGlobalBelegung) $ raumBelegungen plan

sortiereBetreuerBelegungen :: GlobalStundenplan -> [BetreuerBelegung]
sortiereBetreuerBelegungen plan = sortWith (zeit . gbZeiteinheit . bGlobalBelegung) $ betreuerBelegungen plan

schreibeLokalenPlan :: LokalStundenplan -> LaTeXT_ IO
schreibeLokalenPlan lokalerStundenplan = do
  let schuelerplan = map (schreibePlanSchueler lokalerStundenplan) $ (schuelerInnen.seminar.globalStundenplan) lokalerStundenplan
  mconcat schuelerplan

personToTex :: Person -> LaTeXT_ IO
personToTex p = center $ large $ textbf $ fromString $ "Stundenplan von " ++ personZuName p

schreibePlanSchueler :: LokalStundenplan -> SchuelerIn -> LaTeXT_ IO
schreibePlanSchueler lokalerStundenplan schueler = do
  let tage = groupWith (zTag . zeiteinheitZuZeitspanne) $ zeiteinheiten $ seminar $ globalStundenplan $ lokalerStundenplan
  mconcat $
    personToTex (sPerson schueler) :
    map (schreibeTag lokalerStundenplan schueler) tage ++
    [newpage]

schreibeTag :: LokalStundenplan -> SchuelerIn -> [Zeiteinheit] -> LaTeXT_ IO
schreibeTag lokalerStundenplan schueler einheiten=do
  let uberschrift = (large.textbf.fromString.zTag.zeiteinheitZuZeitspanne.head) einheiten
  let tab = mconcat $  [raw "\\begin{tabular} {|p{3cm} p{6cm} p{6cm|} }",  hline]
                    ++ map (schreibeThema lokalerStundenplan schueler) einheiten
                    ++ [raw "\\end{tabular}"]
  mconcat [uberschrift, lnbk, tab, vspace (Mm 5), lnbk]

schreibeThema :: LokalStundenplan -> SchuelerIn -> Zeiteinheit -> LaTeXT_ IO
schreibeThema (LokalStundenplan globalerPlan lokaleBelegungen) schueler zeiteinheit=
  if zTyp zeiteinheit == Physikeinheit
    then schreibePhysikeinheit (LokalStundenplan globalerPlan lokaleBelegungen) schueler zeiteinheit
    else mconcat ["Noch" & "Nichts" & "hier", lnbk, hline]

schreibePhysikeinheit :: LokalStundenplan -> SchuelerIn -> Zeiteinheit -> LaTeXT_ IO
schreibePhysikeinheit (LokalStundenplan globalerPlan lokaleBelegungen) schueler zeiteinheit = do
  let lokaleBelegung = filter (\lb -> lSchuelerIn lb == schueler && (gbZeiteinheit.lGlobalBelegung) lb == zeiteinheit) lokaleBelegungen
  if null lokaleBelegung
    then mconcat ["frei" & "" & "", lnbk, hline]
    else schreibePhysikeinheit' globalerPlan zeiteinheit lokaleBelegung


schreibePhysikeinheit' globalerPlan zeiteinheit lokaleBelegung = do
  let thema = gbThema $ lGlobalBelegung $ head lokaleBelegung
  let betreuer = findeBetreuer globalerPlan zeiteinheit thema
  let raum = findeRaum globalerPlan zeiteinheit thema
  mconcat [(textbf . fromString . nTitel) thema & (textbf . fromString . personZuName . bPerson) betreuer & (textbf . fromString . nTitel) raum, lnbk, hline]
