{-# LANGUAGE FlexibleContexts #-}
module LP where

import Control.Monad
import Prelude hiding ((-))

import Control.Monad.LPMonad
import Data.LinearProgram
import Data.LinearProgram.GLPK.Solver

import Stundenplan
import LPUtils

orpheusLPOptionen :: GLPOpts
orpheusLPOptionen = mipDefaults
  { tmLim = 1000 -- Nach 1000 Sekunden
  , mipGap = 0.1 -- 10 % Abstand zum Optimum sind ausreichend
  }

-- TODO Ist Double auch für die ganzzahligen Werte ok?
type LPSeminarFun = Seminar -> LPM String Double ()

-- TODO ListT?
testLP :: Seminar -> LP String Double
testLP seminar
  = execLPM $ do
    optimum seminar
    global seminar
    lokal seminar

-- | Der optimale Stundenplan wird hier definiert
optimum :: LPSeminarFun
optimum seminar = do
  setDirection Max
  setObjective $ linCombination $
    [ ( praeferenz themenwahl
      , var $ LokalBelegung (GlobalBelegung (gewaehltesThema themenwahl) zeiteinheit) schuelerIn)
      | schuelerIn <- schuelerInnen seminar
      , themenwahl <- themenwahlen schuelerIn
      , zeiteinheit <- zeiteinheiten seminar
    ]

-- | Globale Zwangsbedingungen werden hier definiert
global :: LPSeminarFun
global seminar = do
  -- Ein Thema kann nur stattfinden, wenn BetreuerInnen dafür eingeteilt werden
  forM_ (moeglicheGlobalBelegungen seminar) $ \gb -> do
    setVarKind (var gb) BinVar
    varLF gb `leq` add
      [ varLF $ BetreuerBelegung gb betreuerIn
        | betreuerIn <- betreuerInnen seminar
      ]
  -- BetreuerInnen werden nur eingeteilt, wenn das Thema stattfindet
  forM_ (moeglicheBetreuerBelegungen seminar) $ \bb -> do
    setVarKind (var bb) BinVar
    varLF bb `leq` varLF (bGlobalBelegung bb)
  -- BetreuerInnen können zu einer Zeit höchstens an einem Ort sein
  sequence_ $ do
    betreuerIn <- betreuerInnen seminar
    zeiteinheit <- zeiteinheiten seminar
    return $ add
      [ varLF $ BetreuerBelegung (GlobalBelegung thema zeiteinheit) betreuerIn
        | thema <- themen seminar
      ] `leqTo` 1
  ausnahmeMussStattfindenAn seminar
  -- TODO Raumzuordnungen

ausnahmeMussStattfindenAn :: LPSeminarFun
ausnahmeMussStattfindenAn seminar = sequence_ $ do
  thema <- themen seminar
  zeiteinheit <- mussStattfindenAn thema
  return $ varLF (GlobalBelegung thema zeiteinheit) `equalTo` 1

raumPlanung :: LPSeminarFun
raumPlanung seminar = do
  -- Ein Raum wird nur belegt, wenn dort etwas stattfindet
  forM_ (moeglicheRaumBelegungen seminar) $ \rb -> do
    setVarKind (var rb) BinVar
    varLF rb `leq` varLF (rGlobalBelegung rb)
  -- Für jedes Thema muss ein Raum gebucht sein
  sequence_ $ do
    thema <- themen seminar
    zeiteinheit <- zeiteinheiten seminar
    let gb = GlobalBelegung thema zeiteinheit
    return $ varLF gb `leq` add
      [ varLF $ RaumBelegung gb raum
        | raum <- raeume seminar
      ]
  -- TODO Raumausnahmen und weitere Ausnahmen


-- Lokale (SchülerInnen betreffende) Zwangsbedingungen
lokal :: LPSeminarFun
lokal seminar = do
  -- SchülerInnen werden nur eingeteilt, wenn das Thema dann stattfindet
  forM_ (moeglicheLokalBelegungen seminar) $ \lb -> do
    setVarKind (var lb) BinVar
    varLF lb `leq` varLF (lGlobalBelegung lb)
  -- SchülerInnen können zu einer Zeit höchstens an einem Ort sein
  sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    zeiteinheit <- zeiteinheiten seminar
    return $ add
      [ varLF $ LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn
        | thema <- themen seminar
      ] `leqTo` 1
    -- TODO Eigentlich wollen wir hier sowas wie "trace moeglicheGlobalBelegungen themen"
  -- Jedes Thema wird höchstens einmal belegt
  sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    thema <- themen seminar
    return $ add
      [ varLF $ LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn
        | zeiteinheit <- zeiteinheiten seminar
      ] `leqTo` 1
  --voraussetzungenErzwingen seminar

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
