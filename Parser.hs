{-# LANGUAGE Arrows #-}
module Parser where

import Stundenplan
import Text.XML.HXT.Core
import Data.Maybe
import GHC.Exts (sortWith)

leseSeminar dir = do
  zeiteinheiten <- leseZeiteinheiten dir
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
  let schuelerInnen' = map (fuegeThemenwahlenHinzuS themen themenwahlen) schuelerInnen
  let betreuerInnen' = map (fuegeThemenwahlenHinzuB themen themenwahlen) betreuerInnen
  return $ Seminar (Node 0 "seminar") schuelerInnen' betreuerInnen' themen'' zeiteinheiten raeume'

leseZeiteinheiten dir = runX $ parseXML (dir ++ "zeiteinheiten.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseZeiteinheiten

leseRaeume dir = runX $ parseXML (dir ++ "räume.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseRaeume 

leseThemen dir raeume = runX $ parseXML (dir ++ "themenauswahl.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseThemen raeume

leseVoraussetzungen dir = runX $ parseXML (dir ++ "alle-voraussetzungen.xml") >>> atTag "eck_voraussetzungs" >>> atTag "eck_voraussetzung"  >>> parseVoraussetzungen

leseSchuelerInnen dir =runX $ parseXML (dir ++ "teilnehmer-und-betreuer.xml") >>> atTag "users" >>> atTag "user"  >>> parseSchuelerInnen

leseBetreuerInnen dir =runX $ parseXML (dir ++ "teilnehmer-und-betreuer.xml") >>> atTag "users" >>> atTag "user"  >>> parseBetreuerInnen

leseThemenwahlen dir = runX $ parseXML (dir ++ "themenwahlen.xml") >>> atTag "nodes" >>> atTag "node" >>> parseThemenwahlen

leseMussStattfinden dir = runX $ parseXML (dir ++ "muss-stattfinden-an.xml") >>> atTag "nodes" >>> atTag "node" >>> parseMussStattfinden

leseVerpasst dir = runX $ parseXML (dir ++ "verpassen.xml") >>> atTag "users" >>> atTag "user"  >>> parseVerpasst

leseNichtVerfuegbar dir = runX $ parseXML (dir ++ "raum-nicht-verfügbar.xml") >>> atTag "nodes" >>> atTag "nodes"  >>> parseNichtVerfuegbar


parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file

atTag tag = deep (isElem >>> hasName tag)


parseZeiteinheiten = proc node -> do
  nid <- getText <<<  getChildren <<<  atTag "id" -< node
  titel <- getText <<< getChildren <<< atTag "Titel" -< node
  pe <- getText <<< getChildren <<< atTag "Physikeinheit" -< node
  if pe == "Ja"
    then returnA -< Zeiteinheit (Node (read nid) titel)
    else zeroArrow -< ()

parseRaeume = proc node -> do
  nid <- getText <<<  getChildren <<<  atTag "id" -< node
  titel <- getText <<< getChildren <<< atTag "Name" -< node
  beamer <- getText <<< getChildren <<< atTag "Beamer" -< node
  rgr <- getText <<< getChildren <<< atTag "Raumgr-e" -< node
  let b = if beamer == "Ja" then True else False
  returnA -< Raum (Node (read nid) titel) (read rgr) b []

parseThemen  raeume = proc node -> do
  nid <- getText <<<  getChildren <<<  atTag "id" -< node
  titel <- getText <<< getChildren <<< atTag "Thema" -< node
  raum  <- withDefault  ( arr Just <<< getText <<< getChildren <<< atTag "Raum") Nothing -< node
  beamer <- withDefault (getText <<< getChildren <<< atTag "Beamer") "Nein" -< node
  let b = beamer == "Ja"
  let r  = if raum == Nothing then Nothing else findeRaumById raeume $ read (fromJust raum)
  returnA -<  Thema (Node (read nid) titel) r b [] []

parseVoraussetzungen  = proc node -> do
  voraussetzend <- getText <<<  getChildren <<<  atTag "voraussetzend" -< node
  voraussetzung <- getText <<< getChildren <<< atTag "Voraussetzung" -< node
  returnA -<  (read voraussetzend, read voraussetzung)


parseSchuelerInnen=proc user->do
  uid <- getText <<< getChildren <<<atTag "id" -< user
  vorname <- getText <<< getChildren <<<atTag "Vorname" -< user
  nachname <- getText <<< getChildren <<<atTag "Nachname" -< user
  rollen<- withDefault (getText <<< getChildren <<<atTag "Rollen") "" -< user
  if rollen == ""
    then returnA -< SchuelerIn (Person (read uid) vorname nachname) []
    else zeroArrow -< ()

parseBetreuerInnen=proc user->do
  uid <- getText <<< getChildren <<<atTag "id" -< user
  vorname <- getText <<< getChildren <<<atTag "Vorname" -< user
  nachname <- getText <<< getChildren <<<atTag "Nachname" -< user
  rollen<- withDefault (getText <<< getChildren <<<atTag "Rollen") "" -< user
  if rollen/=""
    then returnA -< BetreuerIn (Person (read uid) vorname nachname) []
    else zeroArrow -< ()

parseVerpasst = proc user->do
  teilnehmerid <- getText <<< getChildren <<< atTag "Benutzer" -< user
  zeiteinheit <- getText <<< getChildren <<<atTag "Zeiteinheit" -< user
  returnA -<  (read teilnehmerid, read zeiteinheit) :: (Integer, Integer)

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

findeRaumById raeume rid =
  if (length rs == 1) then Just (head rs) else Nothing
  where rs = [r|r <- raeume, (nid  (rnode r)) == rid]

findeThemaById themen tid =
  head ts
  where ts = [t|t <- themen, (nid  (tnode t)) == tid]

findeZeiteinheitById zeiteinheiten zid =
  head zs
  where zs = [z|z <- zeiteinheiten, (nid  (znode z)) == zid]

findeVoraussetzungen themen voraussetzungen thema =
  Thema (tnode thema) (raum thema) (tbeamer thema) (mussStattfindenAn thema) liste
  where idlist = [voraussetzung | (voraussetzend, voraussetzung) <- voraussetzungen, voraussetzend == (nid (tnode thema))]
        liste = map (findeThemaById themen) idlist

findeThemenwahlen themen themenwahlen uid = [Themenwahl (findeThemaById themen tid) wahl| (tid, uid', wahl)<- themenwahlen, uid'==uid]

fuegeThemenwahlenHinzuS themen themenwahlen schuelerIn = SchuelerIn (sPerson schuelerIn) (findeThemenwahlen themen themenwahlen (uid (sPerson schuelerIn)))

fuegeThemenwahlenHinzuB themen themenwahlen betreuerIn = BetreuerIn (bPerson betreuerIn) (findeThemenwahlen themen themenwahlen (uid (bPerson betreuerIn)))


findeMussStattfinden zeiteinheiten mussStattfinden thema=Thema (tnode thema) (raum thema) (tbeamer thema) stattfinden (voraussetzungen thema)
  where
    maybeZid=lookup (nid (tnode thema)) mussStattfinden
    stattfinden = case maybeZid of
      Nothing  -> []
      Just zid -> [z|z <- zeiteinheiten, (nid  (znode z)) == zid]
