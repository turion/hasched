{-# LANGUAGE RecordWildCards #-}
module LP.Lokal where

import LP.Imports

import Prelude hiding ((-))

-- Lokale (SchülerInnen betreffende) Zwangsbedingungen
lokal :: LPSeminarFun
lokal seminar = do
  lokalNotwendigkeiten seminar
  lokalSinnvolles seminar
  lokalAusnahmen seminar
  -- TODO Eigentlich wollen wir hier sowas wie "trace moeglicheGlobalBelegungen themen"

-- | Physisch notwendige Zwangsbedingungen
lokalNotwendigkeiten :: LPSeminarFun
lokalNotwendigkeiten seminar = do
  schuelerInnenKoennenSichNichtSpalten seminar
  schuelerInnenNichtUnnoetigEinteilen seminar --TODO: Diese Bedingung ist zu stark?!

-- | SchülerInnen werden nur eingeteilt, wenn das Thema dann stattfindet
schuelerInnenNichtUnnoetigEinteilen :: LPSeminarFun
schuelerInnenNichtUnnoetigEinteilen seminar =
  forM_ (moeglicheLokalBelegungen seminar) $ \lb -> do
    varLF lb `leq` varLF (lGlobalBelegung lb)

-- | SchülerInnen können zu einer Zeit höchstens an einem Ort sein
schuelerInnenKoennenSichNichtSpalten :: LPSeminarFun
schuelerInnenKoennenSichNichtSpalten seminar =
 sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    zeiteinheit <- zeiteinheiten seminar
    return $ add
      [ varLF $ LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn
        | thema <- themen seminar
      ] `leqTo` 1

-- | Zwangsbedingungen, die den Stundenplan höchstwahrscheinlich sinnvoller machen
lokalSinnvolles :: LPSeminarFun
lokalSinnvolles seminar = do
  voraussetzungenErzwingen seminar
  themenNichtDoppeltBelegen seminar

-- | Jedes Thema wird höchstens einmal belegt
themenNichtDoppeltBelegen :: LPSeminarFun
themenNichtDoppeltBelegen seminar =
  sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    thema <- themen seminar
    return $ add
      [ varLF $ LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn
        | zeiteinheit <- zeiteinheiten seminar
      ] `leqTo` 1

-- | Vorraussetzungen fuer ein gewaehltes thema muss der/die SchuelerIn schon belegt haben oder er kennt sie schon
voraussetzungenErzwingen :: LPSeminarFun
voraussetzungenErzwingen seminar = do
  -- Diese Variable gibt an,
  -- ob jemand das Thema zu einer bestimmten Zeit schon besucht hat,
  -- oder angegeben hat, dass er/sie sich damit auskennt (Präferenz 0 Sterne)
  sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    thema <- themen seminar
    zeiteinheit <- zeiteinheiten seminar
    let varKompetent = var ("kompetent", LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn)
    return $ do
      setVarKind varKompetent BinVar
      (asLinFunc varKompetent - add
          [ varLF (GlobalBelegung thema vorher)
            | vorher <- vorherigeZeiteinheiten zeiteinheit $ zeiteinheiten seminar
          ])
        `leqTo` if thema `elem`
          [ gewaehltesThema wahl
            | wahl <- themenwahlen schuelerIn
            , praeferenz wahl == 0
          ]
          then 1 else 0
  -- Wenn ein Thema Voraussetzungen hat,
  -- muss der/die SchuelerIn kompetent darin sein
  sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    thema <- themen seminar
    zeiteinheit <- zeiteinheiten seminar
    voraussetzung <- voraussetzungen thema
    return $
      varLF (LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn)
      `leq` varLF ("kompetent", LokalBelegung (GlobalBelegung voraussetzung zeiteinheit) schuelerIn)


lokalAusnahmen = schuelerInnenVerpassen

schuelerInnenVerpassen :: LPSeminarFun
schuelerInnenVerpassen Seminar {..} = sequence_ $ do
  schuelerIn  <- schuelerInnen
  zeiteinheit <- verpasst $ sPerson schuelerIn
  thema       <- themen
  return $ varLF (LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn) `equalTo` 0
