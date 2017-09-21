{-# LANGUAGE Arrows #-}
module Parser where

import Stundenplan
import Text.XML.HXT.Core
import Data.Maybe
import Text.Read (readMaybe)
import GHC.Exts (sortWith)
import Data.Tree.NTree.TypeDefs
import Data.List.Split

type Voraussetzung = (Nid, Nid)
type Themenwahl' = (Uid, Nid, Double)
type Verpasst = (Uid, Nid)
type MussStattfinden = (Nid, Nid)
type NichtVerfuegbar = (Nid, Nid)

parseXML :: String ->  IOStateArrow s b XmlTree
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file

atTag tag = deep (isElem >>> hasName tag)

textAtTag str = atTag str >>> getChildren >>> getText

leseSeminar :: String -> IO Seminar
leseSeminar dir = do
  zeiteinheiten <- sortWith (zeit) <$> leseZeiteinheiten dir
  raeume <- leseRaeume dir
  nichtVerfuegbar <- leseNichtVerfuegbar dir
  let raeume' = map (findeNichtVerfuegbar zeiteinheiten nichtVerfuegbar) raeume
  themen <- sortWith (nid . tnode) <$> leseThemen dir raeume'
  voraussetzungen <- leseVoraussetzungen dir
  mussStattfinden <- leseMussStattfinden dir
  let themen' = map (findeVoraussetzungen themen voraussetzungen) themen
  let themen'' = map (findeMussStattfinden zeiteinheiten mussStattfinden) themen'
  themenwahlen <- leseThemenwahlen dir
  schuelerInnen <- leseSchuelerInnen dir
  betreuerInnen <- leseBetreuerInnen dir
  verpasst <- leseVerpasst dir
  let schuelerInnen' = map (fuegeThemenwahlenHinzuS themen themenwahlen) schuelerInnen
  let betreuerInnen' = map (fuegeThemenwahlenHinzuB themen themenwahlen) betreuerInnen
  --let schuelerInnen''  = map (fuegeVerpassenHinzuS  zeiteinheiten verpasst) schuelerInnen'
  --let betreuerInnen''  = map (fuegeVerpassenHinzuB  zeiteinheiten verpasst) betreuerInnen'
  return $ Seminar (Node 0 "seminar") schuelerInnen' betreuerInnen' themen'' zeiteinheiten raeume

leseZeiteinheiten :: String -> IO [Zeiteinheit]
leseZeiteinheiten dir = runX $ parseXML (dir ++ "zeiteinheiten.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseZeiteinheiten

leseRaeume :: String -> IO [Raum]
leseRaeume dir = runX $ parseXML (dir ++ "räume.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseRaeume

leseThemen :: String -> [Raum] -> IO [Thema]
leseThemen dir raeume = runX $ parseXML (dir ++ "themenauswahl.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseThemen raeume

leseVoraussetzungen :: String -> IO([Voraussetzung])
leseVoraussetzungen dir = runX $ parseXML (dir ++ "alle-voraussetzungen.xml") >>> atTag "eck_voraussetzungs" >>> atTag "eck_voraussetzung"  >>> parseVoraussetzungen

leseSchuelerInnen :: String -> IO [SchuelerIn]
leseSchuelerInnen dir =runX $ parseXML (dir ++ "teilnehmer-und-betreuer.xml") >>> atTag "users" >>> atTag "user"  >>> parseSchuelerInnen

leseBetreuerInnen :: String -> IO [BetreuerIn]
leseBetreuerInnen dir =runX $ parseXML (dir ++ "teilnehmer-und-betreuer.xml") >>> atTag "users" >>> atTag "user"  >>> parseBetreuerInnen

leseThemenwahlen :: String -> IO [Themenwahl']
leseThemenwahlen dir = runX $ parseXML (dir ++ "themenwahlen.xml") >>> atTag "nodes" >>> atTag "node" >>> parseThemenwahlen

leseVerpasst :: String -> IO [Verpasst]
leseVerpasst dir = runX $ parseXML (dir ++ "verpassen.xml") >>> atTag "users" >>> atTag "user"  >>> parseVerpasst

leseMussStattfinden :: String -> IO [MussStattfinden]
leseMussStattfinden dir = runX $ parseXML (dir ++ "muss-stattfinden-an.xml") >>> atTag "nodes" >>> atTag "node" >>> parseMussStattfinden

leseNichtVerfuegbar :: String -> IO [NichtVerfuegbar]
leseNichtVerfuegbar dir = runX $ parseXML (dir ++ "raum-nicht-verfügbar.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseNichtVerfuegbar


parseZeiteinheiten :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Zeiteinheit
parseZeiteinheiten = proc node -> do
  nid   <- textAtTag "id"    -< node
  titel <- textAtTag "Titel" -< node
  pe    <- withDefault (textAtTag "Physikeinheit") "Nein" -< node
  exk   <- withDefault (textAtTag "Exkursion") "Nein"     -< node
  zeit  <- textAtTag "Zeit"  -< node
  ort   <- withDefault (textAtTag "Ort") "" -< node
  let typ | pe  == "Ja" = Physikeinheit
          | exk == "Ja" = Exkursion
          | otherwise   = Anderes
  returnA -< Zeiteinheit (Node (read nid) titel) typ zeit ort

parseRaeume :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Raum
parseRaeume = proc node -> do
  nid    <- textAtTag "id"       -< node
  titel  <- textAtTag "Name"     -< node
  beamer <- textAtTag "Beamer"   -< node
  rgr    <- textAtTag "Raumgr-e" -< node
  let b = beamer == "Ja"
  returnA -< Raum (Node (read nid) titel) (read rgr) b []


parseThemen :: [Raum] -> IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Thema
parseThemen  raeume = proc node -> do
  nid     <- textAtTag "id"    -< node
  titel   <- textAtTag "Thema" -< node
  mraumId <- withDefault  ( arr Just <<< textAtTag "Raum") Nothing -< node
  beamer  <- withDefault (textAtTag "Beamer") "Nein" -< node
  let
    b = beamer == "Ja"
    raum = do
      raumId <- mraumId
      raumIdInt <- readMaybe raumId
      findeByNid raeume raumIdInt
  returnA -<  Thema (Node (read nid) titel) raum b [] []

parseVoraussetzungen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Voraussetzung
parseVoraussetzungen  = proc node -> do
  voraussetzend <- textAtTag "voraussetzend" -< node
  voraussetzung <- textAtTag "Voraussetzung" -< node
  returnA -< (read voraussetzend, read voraussetzung)

parseSchuelerInnen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) SchuelerIn
parseSchuelerInnen=proc user->do
  uid      <- textAtTag "id"       -< user
  vorname  <- textAtTag "Vorname"  -< user
  nachname <- textAtTag "Nachname" -< user
  rollen   <- withDefault (textAtTag "Rollen") "" -< user
  if rollen == ""
    then returnA -< SchuelerIn (Person (read uid) vorname nachname []) []
    else zeroArrow -< ()

parseBetreuerInnen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) BetreuerIn
parseBetreuerInnen=proc user->do
  uid      <- textAtTag "id"       -< user
  vorname  <- textAtTag "Vorname"  -< user
  nachname <- textAtTag "Nachname" -< user
  rollen   <- withDefault (textAtTag "Rollen") "" -< user
  if rollen /= ""
    then returnA -< BetreuerIn (Person (read uid) vorname nachname []) []
    else zeroArrow -< ()

parseVerpasst :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Verpasst
parseVerpasst = proc user->do
  teilnehmerid  <- textAtTag "Benutzer"    -< user
  zeiteinheitid <- textAtTag "Zeiteinheit" -< user
  returnA -< (read teilnehmerid, read zeiteinheitid)

parseThemenwahlen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Themenwahl'
parseThemenwahlen = proc node -> do
  themaid      <- textAtTag "Thema"    -< node
  teilnehmerid <- textAtTag "Benutzer" -< node
  bewertung    <- textAtTag "Wahl"     -< node
  returnA -< (read teilnehmerid, read themaid, read bewertung)

parseMussStattfinden :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) MussStattfinden
parseMussStattfinden = proc node -> do
  zid <- textAtTag "zid" -< node
  tid <- textAtTag "tid" -< node
  returnA -< (read tid, read zid)

parseNichtVerfuegbar :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) NichtVerfuegbar
parseNichtVerfuegbar = proc node -> do
  zid <- textAtTag "zid" -< node
  rid <- textAtTag "id"  -< node
  returnA -< (read rid, read zid)

findeRaumById :: [Raum] -> Nid -> Raum
findeRaumById = findeByNidError "Raum"

findeThemaById :: [Thema] -> Nid -> Thema
findeThemaById = findeByNidError "Thema"

findeZeiteinheitById :: [Zeiteinheit] -> Nid -> Zeiteinheit
findeZeiteinheitById = findeByNidError "Zeiteinheit"

findeVoraussetzungen :: [Thema] -> [Voraussetzung] -> Thema -> Thema
findeVoraussetzungen themen voraussetzungen thema = thema { voraussetzungen = liste }
  where
    liste =
      [ findeThemaById themen voraussetzung
        | (voraussetzend, voraussetzung) <- voraussetzungen
        , voraussetzend == nodeId thema
      ]

findeThemenwahlen :: [Thema] -> [Themenwahl'] -> Uid -> [Themenwahl]
findeThemenwahlen themen themenwahlen uid =
  [ Themenwahl (findeThemaById themen tid) wahl
    | (uid', tid, wahl) <- themenwahlen
    , uid' == uid
  ]

fuegeThemenwahlenHinzuS :: [Thema] -> [Themenwahl'] -> SchuelerIn -> SchuelerIn
fuegeThemenwahlenHinzuS themen themenwahlen schuelerIn = schuelerIn { themenwahlen = findeThemenwahlen themen themenwahlen $ uid $ sPerson schuelerIn }

fuegeThemenwahlenHinzuB :: [Thema] -> [Themenwahl'] -> BetreuerIn -> BetreuerIn
fuegeThemenwahlenHinzuB themen themenwahlen betreuerIn = betreuerIn { betreuteThemen = findeThemenwahlen themen themenwahlen $ uid $ bPerson betreuerIn }


findeVerpassen :: [Zeiteinheit] -> [Verpasst] -> Uid -> [Zeiteinheit]
findeVerpassen zeiteinheiten verpassen uid =
  [ findeZeiteinheitById zeiteinheiten zid
    | (uid', zid) <- verpassen
    , uid' == uid
  ]

fuegeVerpassenHinzuS :: [Zeiteinheit] -> [Verpasst] -> SchuelerIn -> SchuelerIn
fuegeVerpassenHinzuS zeiteinheiten verpassen schuelerIn = schuelerIn { sPerson = (sPerson schuelerIn) { verpasst = verpassteEinheiten } }
 where verpassteEinheiten = findeVerpassen zeiteinheiten verpassen $ uid $ sPerson schuelerIn

fuegeVerpassenHinzuB :: [Zeiteinheit] -> [Verpasst] -> BetreuerIn -> BetreuerIn
fuegeVerpassenHinzuB zeiteinheiten verpassen betreuerIn = betreuerIn { bPerson = (bPerson betreuerIn) { verpasst = verpassteEinheiten } }
  where verpassteEinheiten = findeVerpassen zeiteinheiten verpassen $ uid $ bPerson betreuerIn


findeNichtVerfuegbar :: [Zeiteinheit] -> [NichtVerfuegbar] -> Raum -> Raum
findeNichtVerfuegbar zeiteinheiten nichtVerfuegbare raum = raum { nichtVerfuegbar = liste }
  where
    liste =
      [ findeZeiteinheitById zeiteinheiten zid
        | (rid', zid) <- nichtVerfuegbare
        , rid' == nodeId raum
      ]


findeMussStattfinden zeiteinheiten mussStattfinden thema = thema { mussStattfindenAn = stattfinden }
  where
    maybeZid = lookup (nodeId thema) mussStattfinden
    stattfinden = maybe
      []
      (\zid -> filter (matchNid zid) zeiteinheiten)
      maybeZid
      
      
      
      
leseGlobalenPlan :: Seminar -> String -> IO GlobalStundenplan
leseGlobalenPlan seminar file = do
  xml <- leseXML file
  return $ GlobalStundenplan seminar (map (votragZuGlobalBelegung seminar) xml) (map (vortragZuBetreuerBelegung seminar) xml) (map (vortragZuRaumBelegung seminar) xml) "V2.0"
  
  
votragZuGlobalBelegung :: Seminar -> (Int,String,String,String) -> GlobalBelegung
votragZuGlobalBelegung seminar (ze,thema,_,_)=GlobalBelegung (findeThemaByName seminar thema) (findeZeiteinheitByNo seminar ze)

vortragZuBetreuerBelegung :: Seminar -> (Int,String,String,String) -> BetreuerBelegung
vortragZuBetreuerBelegung seminar (ze,thema,betreuer,raum) = BetreuerBelegung (votragZuGlobalBelegung seminar (ze,thema,betreuer,raum)) (findeBetreuerByName seminar betreuer)

vortragZuRaumBelegung :: Seminar -> (Int,String,String,String) -> RaumBelegung
vortragZuRaumBelegung seminar (ze,thema,betreuer,raum) = RaumBelegung (votragZuGlobalBelegung seminar (ze,thema,betreuer,raum)) (findeRaumByName seminar raum)

findeZeiteinheitByNo :: Seminar -> Int -> Zeiteinheit
findeZeiteinheitByNo seminar ze = (filter (\ze -> (zTyp ze)==Physikeinheit) (zeiteinheiten seminar)) !! (ze-1)

findeThemaByName :: Seminar -> String -> Thema
findeThemaByName seminar name =
  let filt = filter (\t -> (titel (tnode t))==name) (themen seminar)
  in if (length filt)==1 then head filt else error ("Fehler bei "++name)
  
findeBetreuerByName :: Seminar -> String -> BetreuerIn
findeBetreuerByName seminar name =
  let nachname' = last $ splitOn " " name
      filt = filter (\b -> (nachname (bPerson b))==nachname') (betreuerInnen seminar)
  in if (length filt)==1 then head filt else error ("Fehler bei "++nachname') 

findeRaumByName :: Seminar -> String -> Raum
findeRaumByName seminar name =
  let filt = filter (\r -> (titel (rnode r))==name) (raeume seminar)
  in if (length filt)==1 then head filt else error ("Fehler bei "++name)

leseXML :: String -> IO [(Int,String,String,String)]
leseXML file =runX $ parseXML file >>> atTag "einheiten" >>> atTag "vortrag" >>> parseVortrag

  
parseVortrag :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) (Int,String,String,String)
parseVortrag = proc vortrag -> do
  einheit <- textAtTag "einheit" -< vortrag
  thema <- textAtTag "thema" -< vortrag
  betreuer <- textAtTag "betreuer" -< vortrag
  raum <- textAtTag "raum" -< vortrag
  returnA -< (read einheit, thema,betreuer,raum)

