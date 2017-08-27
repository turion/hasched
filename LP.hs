{-# LANGUAGE FlexibleContexts #-}
module LP where

import Control.Monad

import Control.Monad.LPMonad
import Data.LinearProgram

import Stundenplan
import LPUtils

-- TODO ListT?
-- TODO Modularisieren
testLP :: Seminar -> LP String Double
testLP seminar
  = execLPM $ do
    optimum seminar
    global seminar
    lokal seminar

-- | Der optimale Stundenplan wird hier definiert
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

-- Lokale (SchülerInnen betreffende) Zwangsbedingungen
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
