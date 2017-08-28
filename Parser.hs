{-# LANGUAGE Arrows #-}
module Parser where

import Stundenplan
import Text.XML.HXT.Core
import Data.Maybe
import Text.Read (readMaybe)
import GHC.Exts (sortWith)
import Data.Tree.NTree.TypeDefs

type Voraussetzung = (Integer, Integer)
type Themenwahl' = (Integer, Integer, Double)
type Verpasst = (Integer, Integer)
type MussStattfinden = (Integer, Integer)
type NichtVerfuegbar = (Integer, Integer)

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
  let schuelerInnen''  = map (fuegeVerpassenHinzuS  zeiteinheiten verpasst) schuelerInnen
  let betreuerInnen''  = map (fuegeVerpassenHinzuB  zeiteinheiten verpasst) betreuerInnen
  return $ Seminar (Node 0 "seminar") schuelerInnen'' betreuerInnen'' themen'' zeiteinheiten raeume

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
leseNichtVerfuegbar dir = runX $ parseXML (dir ++ "raum-nicht-verfügbar.xml") >>> atTag "nodes" >>> atTag "nodes"  >>> parseNichtVerfuegbar


parseZeiteinheiten :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Zeiteinheit
parseZeiteinheiten = proc node -> do
  nid   <- textAtTag "id"    -< node
  titel <- textAtTag "Titel" -< node
  pe    <- withDefault (textAtTag "Physikeinheit") "Nein" -< node
  exk   <- withDefault (textAtTag "Exkursion") "Nein"     -< node
  zeit  <- textAtTag "Zeit"  -< node
  let typ | pe  == "Ja" = Physikeinheit
          | exk == "Ja" = Exkursion
          | otherwise   = Anderes
  returnA -< Zeiteinheit (Node (read nid) titel) typ zeit

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
      findeRaumById raeume raumIdInt
  returnA -<  Thema (Node (read nid) titel) raum b [] []

parseVoraussetzungen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Voraussetzung
parseVoraussetzungen  = proc node -> do
  voraussetzend <- textAtTag "voraussetzend" -< node
  voraussetzung <- textAtTag "Voraussetzung" -< node
  returnA -<  (read voraussetzend, read voraussetzung)

parseSchuelerInnen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) SchuelerIn
parseSchuelerInnen=proc user->do
  uid      <- textAtTag "id"       -< user
  vorname  <- textAtTag"Vorname"   -< user
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
  returnA -< (read themaid, read teilnehmerid, read bewertung)

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


findeRaumById :: [Raum] -> Integer -> Maybe Raum
findeRaumById raeume rid =
  if (length rs == 1) then Just (head rs) else Nothing
  where rs = [ r | r <- raeume, (nid  (rnode r)) == rid]

findeThemaById :: [Thema] -> Integer -> Thema
findeThemaById themen tid =
  head ts
  where ts = [ t | t <- themen, (nid  (tnode t)) == tid]

findeZeiteinheitById :: [Zeiteinheit] -> Integer -> Zeiteinheit
findeZeiteinheitById zeiteinheiten zid =
  head zs
  where zs = [ z | z <- zeiteinheiten, (nid  (znode z)) == zid]

findeVoraussetzungen :: [Thema] -> [Voraussetzung] -> Thema -> Thema
findeVoraussetzungen themen voraussetzungen thema = thema { voraussetzungen = liste }
  where
    liste =
      [ findeThemaById themen voraussetzung
        | (voraussetzend, voraussetzung) <- voraussetzungen
        , voraussetzend == (nid (tnode thema))
      ]

findeThemenwahlen :: [Thema] -> [Themenwahl'] -> Integer -> [Themenwahl]
findeThemenwahlen themen themenwahlen uid =
  [ Themenwahl (findeThemaById themen tid) wahl
    | (tid, uid', wahl) <- themenwahlen
    , uid'==uid
  ]

fuegeThemenwahlenHinzuS :: [Thema] -> [Themenwahl'] -> SchuelerIn -> SchuelerIn
fuegeThemenwahlenHinzuS themen themenwahlen schuelerIn = schuelerIn { themenwahlen = findeThemenwahlen themen themenwahlen (uid (sPerson schuelerIn)) }

fuegeThemenwahlenHinzuB :: [Thema] -> [Themenwahl'] -> BetreuerIn -> BetreuerIn
fuegeThemenwahlenHinzuB themen themenwahlen betreuerIn = betreuerIn { betreuteThemen = findeThemenwahlen themen themenwahlen (uid (bPerson betreuerIn)) }


findeVerpassen :: [Zeiteinheit] -> [Verpasst] -> Integer -> [Zeiteinheit]
findeVerpassen zeiteinheiten verpassen uid =
  [ (findeZeiteinheitById zeiteinheiten zid)
    | (zid, uid') <- verpassen
    , uid'==uid
  ]

fuegeVerpassenHinzuS :: [Zeiteinheit] -> [Verpasst] -> SchuelerIn -> SchuelerIn
fuegeVerpassenHinzuS zeiteinheiten verpassen schuelerIn = schuelerIn { sPerson = (sPerson schuelerIn) { verpasst = verpassteEinheiten } }
 where verpassteEinheiten = findeVerpassen zeiteinheiten verpassen $ uid $ sPerson schuelerIn

fuegeVerpassenHinzuB :: [Zeiteinheit] -> [Verpasst] -> BetreuerIn -> BetreuerIn
fuegeVerpassenHinzuB zeiteinheiten verpassen betreuerIn = betreuerIn { bPerson = (bPerson betreuerIn) { verpasst = verpassteEinheiten } }
 where verpassteEinheiten = findeVerpassen zeiteinheiten verpassen $ uid $ bPerson betreuerIn


findeNichtVerfuegbar :: [Zeiteinheit] -> [(Integer, Integer)] -> Raum -> Raum
findeNichtVerfuegbar zeiteinheiten nichtVerfuegbare raum = raum { nichtVerfuegbar = liste }
  where
    liste =
      [ findeZeiteinheitById zeiteinheiten zid
        | (rid', zid) <- nichtVerfuegbare
        , rid' == nid (rnode raum)
      ]


findeMussStattfinden zeiteinheiten mussStattfinden thema = thema { mussStattfindenAn = stattfinden }
  where
    maybeZid = lookup (nid (tnode thema)) mussStattfinden
    stattfinden = maybe
      []
      (\zid -> [ z | z <- zeiteinheiten, (nid  (znode z)) == zid])
      maybeZid
