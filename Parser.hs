{-# LANGUAGE Arrows #-}
module Parser where

import Stundenplan
import Text.XML.HXT.Core
import Data.Maybe
import GHC.Exts (sortWith)
 


leseSeminar dir = do
  zeiteinheiten <- leseZeiteinheiten dir
  raeume <- leseRaeume dir
  themen <- sortWith (nid . tnode) <$> leseThemen dir raeume
  voraussetzungen <- leseVoraussetzungen dir
  mussStattfinden <- leseMussStattfinden dir
  let themen' = map (findeVoraussetzungen themen voraussetzungen) themen
  let themen'' = map (findeMussStattfinden zeiteinheiten mussStattfinden) themen'
  themenwahlen <- leseThemenwahlen dir
  schuelerInnen <- leseSchuelerInnen dir
  betreuerInnen <- leseBetreuerInnen dir
  let schuelerInnen' = map (fuegeThemenwahlenHinzuS themen themenwahlen) schuelerInnen
  let betreuerInnen' = map (fuegeThemenwahlenHinzuB themen themenwahlen) betreuerInnen
  return $ Seminar (Node 0 "seminar") schuelerInnen' betreuerInnen' themen'' zeiteinheiten raeume

leseZeiteinheiten dir = runX $ parseXML (dir ++ "zeiteinheiten.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseZeiteinheiten

leseRaeume dir = runX $ parseXML (dir ++ "räume.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseRaeume

leseThemen dir raeume = runX $ parseXML (dir ++ "themenauswahl.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseThemen raeume

leseVoraussetzungen dir = runX $ parseXML (dir ++ "alle-voraussetzungen.xml") >>> atTag "eck_voraussetzungs" >>> atTag "eck_voraussetzung"  >>> parseVoraussetzungen

leseSchuelerInnen dir =runX $ parseXML (dir ++ "teilnehmer-und-betreuer.xml") >>> atTag "users" >>> atTag "user"  >>> parseSchuelerInnen

leseBetreuerInnen dir =runX $ parseXML (dir ++ "teilnehmer-und-betreuer.xml") >>> atTag "users" >>> atTag "user"  >>> parseBetreuerInnen

leseThemenwahlen dir = runX $ parseXML (dir ++ "themenwahlen.xml") >>> atTag "nodes" >>> atTag "node" >>> parseThemenwahlen

leseMussStattfinden dir = runX $ parseXML (dir ++ "muss-stattfinden-an.xml") >>> atTag "nodes" >>> atTag "node" >>> parseMussStattfinden

leseVerpasst dir = runX $ parseXML (dir ++ "verpassen.xml") >>> atTag "users" >>> atTag "user"  >>> parseVerpasst


parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file

atTag tag = deep (isElem >>> hasName tag)

textAtTag = atTag >>> getChildren <<< getText

parseZeiteinheiten = proc node -> do
  nid <- textAtTag "id" -< node
  titel <- textAtTag "Titel" -< node
  pe <- textAtTag "Physikeinheit" -< node
  if pe == "Ja"
    then returnA -< Zeiteinheit (Node (read nid) titel)
    else zeroArrow -< ()

parseRaeume = proc node -> do
  nid <- textAtTag "id" -< node
  titel <- textAtTag "Name" -< node
  beamer <- textAtTag "Beamer" -< node
  rgr <- textAtTag "Raumgr-e" -< node
  let b = if beamer == "Ja" then True else False
  returnA -< Raum (Node (read nid) titel) (read rgr) b


parseThemen  raeume = proc node -> do
  nid <- textAtTag "id" -< node
  titel <- textAtTag "Thema" -< node
  raum  <- withDefault  ( arr Just <<< textAtTag "Raum") Nothing -< node
  beamer <- withDefault (textAtTag "Beamer") "Nein" -< node
  let b = beamer == "Ja"
  let r  = if raum == Nothing then Nothing else findeRaumById raeume $ read (fromJust raum)
  returnA -<  Thema (Node (read nid) titel) r b [] []

parseVoraussetzungen  = proc node -> do
  voraussetzend <- textAtTag "voraussetzend" -< node
  voraussetzung <- textAtTag "Voraussetzung" -< node
  returnA -<  (read voraussetzend, read voraussetzung)


parseSchuelerInnen=proc user->do
  uid <- textAtTag "id" -< user
  vorname <- textAtTag "Vorname" -< user
  nachname <- textAtTag "Nachname" -< user
  rollen<- withDefault (textAtTag "Rollen") "" -< user
  if rollen == ""
    then returnA -< SchuelerIn (Person (read uid) vorname nachname) []
    else zeroArrow -< ()

parseBetreuerInnen=proc user->do
  uid <- textAtTag "id" -< user
  vorname <- textAtTag "Vorname" -< user
  nachname <- textAtTag "Nachname" -< user
  rollen<- withDefault (textAtTag "Rollen") "" -< user
  if rollen/=""
    then returnA -< BetreuerIn (Person (read uid) vorname nachname) []
    else zeroArrow -< ()

parseVerpasst = proc user->do
  teilnehmerid <- textAtTag "Benutzer" -< user
  zeiteinheit <- textAtTag "Zeiteinheit" -< user
  returnA -<  (read teilnehmerid, read zeiteinheit) :: (Integer, Integer)

parseThemenwahlen = proc node -> do
  themaid <- textAtTag "Thema" -< node
  teilnehmerid <- textAtTag "Benutzer" -< node
  bewertung <- textAtTag "Wahl" -< node
  returnA -< (read themaid, read teilnehmerid, read bewertung) :: (Integer, Integer, Double)

parseMussStattfinden = proc node -> do
  zid <- textAtTag "zid" -< node
  tid <- textAtTag "tid" -< node
  returnA -< (read tid, read zid) :: (Integer, Integer)

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
