{-# LANGUAGE Arrows #-}
module Parser where

import Stundenplan
import Text.XML.HXT.Core
import Data.Maybe
import GHC.Exts (sortWith)
import Data.Tree.NTree.TypeDefs


type Voraussetzung = (Integer, Integer)
type Themenwahl' = (Integer, Integer, Double)
type Verpasst = (Integer, Integer)

leseSeminar :: String -> IO(Seminar)
leseSeminar dir = do
  zeiteinheiten <- leseZeiteinheiten dir
  -- TODO Zeiteinheiten müssen chronologisch sortiert werden!
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

leseZeiteinheiten :: String -> IO([Zeiteinheit])
leseZeiteinheiten dir = runX $ parseXML (dir ++ "zeiteinheiten.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseZeiteinheiten

leseRaeume :: String -> IO([Raum])
leseRaeume dir = runX $ parseXML (dir ++ "räume.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseRaeume

leseThemen :: String -> [Raum] -> IO([Thema])
leseThemen dir raeume = runX $ parseXML (dir ++ "themenauswahl.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseThemen raeume

leseVoraussetzungen :: String -> IO([Voraussetzung])
leseVoraussetzungen dir = runX $ parseXML (dir ++ "alle-voraussetzungen.xml") >>> atTag "eck_voraussetzungs" >>> atTag "eck_voraussetzung"  >>> parseVoraussetzungen

leseSchuelerInnen :: String -> IO([SchuelerIn])
leseSchuelerInnen dir =runX $ parseXML (dir ++ "teilnehmer-und-betreuer.xml") >>> atTag "users" >>> atTag "user"  >>> parseSchuelerInnen

leseBetreuerInnen :: String -> IO([BetreuerIn])
leseBetreuerInnen dir =runX $ parseXML (dir ++ "teilnehmer-und-betreuer.xml") >>> atTag "users" >>> atTag "user"  >>> parseBetreuerInnen

leseThemenwahlen :: String -> IO([Themenwahl'])
leseThemenwahlen dir = runX $ parseXML (dir ++ "themenwahlen.xml") >>> atTag "nodes" >>> atTag "node" >>> parseThemenwahlen

leseVerpasst :: String -> IO([Verpasst]) 
leseVerpasst dir = runX $ parseXML (dir ++ "verpassen.xml") >>> atTag "users" >>> atTag "user"  >>> parseVerpasst

leseMussStattfinden dir = runX $ parseXML (dir ++ "muss-stattfinden-an.xml") >>> atTag "nodes" >>> atTag "node" >>> parseMussStattfinden

leseNichtVerfuegbar dir = runX $ parseXML (dir ++ "raum-nicht-verfügbar.xml") >>> atTag "nodes" >>> atTag "nodes"  >>> parseNichtVerfuegbar

parseXML :: String ->  IOStateArrow s b XmlTree
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file

atTag tag = deep (isElem >>> hasName tag)



parseZeiteinheiten :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Zeiteinheit
parseZeiteinheiten = proc node -> do
  nid <- getText <<<  getChildren <<<  atTag "id" -< node
  titel <- getText <<< getChildren <<< atTag "Titel" -< node
  pe <- getText <<< getChildren <<< atTag "Physikeinheit" -< node
  if pe == "Ja"
    then returnA -< Zeiteinheit (Node (read nid) titel)
    else zeroArrow -< ()

parseRaeume :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Raum
parseRaeume = proc node -> do
  nid <- getText <<<  getChildren <<<  atTag "id" -< node
  titel <- getText <<< getChildren <<< atTag "Name" -< node
  beamer <- getText <<< getChildren <<< atTag "Beamer" -< node
  rgr <- getText <<< getChildren <<< atTag "Raumgr-e" -< node
  let b = if beamer == "Ja" then True else False
  returnA -< Raum (Node (read nid) titel) (read rgr) b []


parseThemen :: [Raum] -> IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Thema  
parseThemen  raeume = proc node -> do
  nid <- getText <<<  getChildren <<<  atTag "id" -< node
  titel <- getText <<< getChildren <<< atTag "Thema" -< node
  raum  <- withDefault  ( arr Just <<< getText <<< getChildren <<< atTag "Raum") Nothing -< node
  beamer <- withDefault (getText <<< getChildren <<< atTag "Beamer") "Nein" -< node
  let b = beamer == "Ja"
  let r  = if raum == Nothing then Nothing else findeRaumById raeume $ read (fromJust raum)
  returnA -<  Thema (Node (read nid) titel) r b [] []

parseVoraussetzungen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Voraussetzung
parseVoraussetzungen  = proc node -> do
  voraussetzend <- getText <<<  getChildren <<<  atTag "voraussetzend" -< node
  voraussetzung <- getText <<< getChildren <<< atTag "Voraussetzung" -< node
  returnA -<  (read voraussetzend, read voraussetzung)    

parseSchuelerInnen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) SchuelerIn
parseSchuelerInnen=proc user->do
  uid <- getText <<< getChildren <<<atTag "id" -< user
  vorname <- getText <<< getChildren <<<atTag "Vorname" -< user
  nachname <- getText <<< getChildren <<<atTag "Nachname" -< user
  rollen<- withDefault (getText <<< getChildren <<<atTag "Rollen") "" -< user
  if rollen == "" 
    then returnA -< SchuelerIn (Person (read uid) vorname nachname []) [] 
    else zeroArrow -< ()

parseBetreuerInnen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) BetreuerIn
parseBetreuerInnen=proc user->do
  uid <- getText <<< getChildren <<<atTag "id" -< user
  vorname <- getText <<< getChildren <<<atTag "Vorname" -< user
  nachname <- getText <<< getChildren <<<atTag "Nachname" -< user
  rollen<- withDefault (getText <<< getChildren <<<atTag "Rollen") "" -< user
  if rollen/="" 
    then returnA -< BetreuerIn (Person (read uid) vorname nachname []) [] 
    else zeroArrow -< ()
  
parseVerpasst :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Verpasst
parseVerpasst = proc user->do
  teilnehmerid <- getText <<< getChildren <<< atTag "Benutzer" -< user
  zeiteinheitid <- getText <<< getChildren <<< atTag "Zeiteinheit" -< user
  returnA -<  (read (teilnehmerid :: String), read zeiteinheitid) :: (Integer, Integer)

parseThemenwahlen :: IOSLA (XIOState ()) (Data.Tree.NTree.TypeDefs.NTree XNode) Themenwahl'
parseThemenwahlen = proc node -> do
  themaid <- getText <<< getChildren <<< atTag "Thema" -< node
  teilnehmerid <- getText <<< getChildren <<< atTag "Benutzer" -< node
  bewertung <- getText <<< getChildren <<< atTag "Wahl" -< node
  returnA -< (read themaid, read teilnehmerid, read bewertung) :: (Integer, Integer, Double)
  
parseMussStattfinden = proc node -> do
  zid <- getText <<< getChildren <<< atTag "zid" -< node
  tid <- getText <<< getChildren <<< atTag "tid" -< node
  returnA -< (read tid, read zid) :: (Integer, Integer)
  
parseNichtVerfuegbar = proc node -> do
  zid <- getText <<< getChildren <<< atTag "zid" -< node
  rid <- getText <<< getChildren <<< atTag "id" -< node
  returnA -< (read rid, read zid) :: (Integer, Integer)



findeRaumById :: [Raum] -> Integer -> Maybe Raum
findeRaumById raeume rid = 
  if (length rs == 1) then Just (head rs) else Nothing
  where rs = [r|r <- raeume, (nid  (rnode r)) == rid]

findeThemaById :: [Thema] -> Integer -> Thema
findeThemaById themen tid = 
  head ts
  where ts = [t|t <- themen, (nid  (tnode t)) == tid]

findeZeiteinheitById :: [Zeiteinheit] -> Integer -> Zeiteinheit
findeZeiteinheitById zeiteinheiten zid = 
  head zs
  where zs = [z|z <- zeiteinheiten, (nid  (znode z)) == zid]

findeVoraussetzungen :: [Thema] -> [Voraussetzung] -> Thema -> Thema
findeVoraussetzungen themen voraussetzungen thema = 
  Thema (tnode thema) (raum thema) (tbeamer thema) [] liste
  where idlist = [voraussetzung | (voraussetzend, voraussetzung) <- voraussetzungen, voraussetzend == (nid (tnode thema))] 
        liste = map (findeThemaById themen) idlist
       
findeThemenwahlen :: [Thema] -> [Themenwahl'] -> Integer -> [Themenwahl] 
findeThemenwahlen themen themenwahlen uid = [Themenwahl (findeThemaById themen tid) wahl| (tid, uid', wahl)<- themenwahlen, uid'==uid]

fuegeThemenwahlenHinzuS :: [Thema] -> [Themenwahl'] -> SchuelerIn -> SchuelerIn
fuegeThemenwahlenHinzuS themen themenwahlen schuelerIn = SchuelerIn (sPerson schuelerIn) (findeThemenwahlen themen themenwahlen (uid (sPerson schuelerIn)))

fuegeThemenwahlenHinzuB :: [Thema] -> [Themenwahl'] -> BetreuerIn -> BetreuerIn
fuegeThemenwahlenHinzuB themen themenwahlen betreuerIn = BetreuerIn (bPerson betreuerIn) (findeThemenwahlen themen themenwahlen (uid (bPerson betreuerIn)))


findeVerpassen :: [Zeiteinheit] -> [Verpasst] -> Integer -> [Zeiteinheit]
findeVerpassen zeiteinheiten verpassen uid = [(findeZeiteinheitById zeiteinheiten zid) | (zid, uid')<- verpassen, uid'==uid]

fuegeVerpassenHinzuS :: [Zeiteinheit] -> [Verpasst] -> SchuelerIn -> SchuelerIn
fuegeVerpassenHinzuS zeiteinheiten verpassen schuelerIn = schuelerIn {sPerson = (sPerson schuelerIn) {verpasst = verpassteEinheiten}} 
 where verpassteEinheiten = findeVerpassen zeiteinheiten verpassen (uid (sPerson schuelerIn)) 

fuegeVerpassenHinzuB :: [Zeiteinheit] -> [Verpasst] -> BetreuerIn -> BetreuerIn
fuegeVerpassenHinzuB zeiteinheiten verpassen betreuerIn = betreuerIn {bPerson = (bPerson betreuerIn) {verpasst = verpassteEinheiten}} 
 where verpassteEinheiten = findeVerpassen zeiteinheiten verpassen (uid (bPerson betreuerIn)) 


findeNichtVerfuegbar :: [Zeiteinheit] -> [(Integer, Integer)] -> Raum -> Raum
findeNichtVerfuegbar zeiteinheiten nichtVerfuegbar raum =raum {nichtVerfuegbar = liste}
  where liste = map (findeZeiteinheitById zeiteinheiten) [zid | (rid',zid) <- nichtVerfuegbar , rid'==( nid (rnode raum))] 


findeMussStattfinden zeiteinheiten mussStattfinden thema=Thema (tnode thema) (raum thema) (tbeamer thema) stattfinden (voraussetzungen thema)
  where
    maybeZid=lookup (nid (tnode thema)) mussStattfinden
    stattfinden = case maybeZid of
      Nothing  -> []
      Just zid -> [z|z <- zeiteinheiten, (nid  (znode z)) == zid]
