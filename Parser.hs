{-# LANGUAGE Arrows #-}
module Parser where

import Stundenplan
import Text.XML.HXT.Core
import Data.Maybe

leseSeminar dir = do 
  zeiteinheiten <- leseZeiteinheiten dir
  raeume <- leseRaeume dir
  themen <- leseThemen dir raeume
  return $ Seminar (Node 0 "seminar") [] [] themen zeiteinheiten raeume

leseZeiteinheiten dir = runX $ parseXML (dir ++ "zeiteinheiten.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseZeiteinheiten

leseRaeume dir = runX $ parseXML (dir ++ "räume.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseRaeume

leseThemen dir raeume = runX $ parseXML (dir ++ "themenauswahl.xml") >>> atTag "nodes" >>> atTag "node"  >>> parseThemen raeume


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
  returnA -< Raum (Node (read nid) titel) (read rgr) b 

  
parseThemen  raeume = proc node -> do
  nid <- getText <<<  getChildren <<<  atTag "id" -< node
  titel <- getText <<< getChildren <<< atTag "Thema" -< node
  raum  <- withDefault  ( arr Just <<< getText <<< getChildren <<< atTag "Raum") Nothing -< node 
  beamer <- getText <<< getChildren <<< atTag "Beamer" -< node
  let b = beamer == "Ja"
  let r  = if raum == Nothing then Nothing else findeRaumById raeume $ read (fromJust raum)
  returnA -<  Thema (Node (read nid) titel) r b []  
--  returnA -< raum

findeRaumById raeume rid = 
  if (length rs == 1) then Just (head rs) else Nothing
  where rs = [r|r <- raeume, (nid  (rnode r)) == rid]
