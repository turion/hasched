module LP where

import Control.Monad

import Control.Monad.LPMonad
import Data.LinearProgram

import Stundenplan
import LPUtils

-- TODO ListT?
-- TODO Modularisieren
testLP :: Seminar -> LPM String Double ()
testLP seminar@(Seminar _ schuelerInnen betreuerInnen themen zeiteinheiten raeume) = do
  forM_ (moeglicheGlobalBelegungen seminar) $ \gb -> setVarKind (var gb) BinVar
  forM_ (moeglicheBetreuerBelegungen seminar) $ \bb -> do
    setVarKind (var bb) BinVar
    asLinFunc (var bb) `leq` asLinFunc (var (bGlobalBelegung bb))
  sequence_ $ do
    betreuerIn <- betreuerInnen
    zeiteinheit <- zeiteinheiten
    return $ add [ asLinFunc ( var ( BetreuerBelegung (GlobalBelegung thema zeiteinheit) betreuerIn)) | thema <- themen] `leqTo` 1
  forM_ (moeglicheLokalBelegungen seminar) $ \lb -> do
    setVarKind (var lb) BinVar
    asLinFunc (var lb) `leq` asLinFunc (var (lGlobalBelegung lb))
  sequence_ $ do
    schuelerIn <- schuelerInnen
    zeiteinheit <- zeiteinheiten
    return $ add [asLinFunc (var (LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn)) | thema <- themen] `leqTo` 1
    -- TODO Eigentlich wollen wir hier sowas wie "trace moeglicheGlobalBelegungen themen"
