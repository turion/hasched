module Main where

import Control.Monad

import Control.Monad.LPMonad
import Data.LinearProgram

import Stundenplan
import LPUtils
import Parser

testthemen :: [ Thema ]
testthemen = [ Thema (Node 23 "Mondflug") []
             , Thema (Node 42 "Supernova") [testthemen !! 0]
             ]

testzeiteinheiten :: [ Zeiteinheit ]
testzeiteinheiten = [ Zeiteinheit (Node 100 "Z0")
                    , Zeiteinheit (Node 101 "Z1")
                    ]

testseminar :: Seminar
testseminar = Seminar
  (Node 0 "Testseminar")
  [ SchuelerIn (Person 100000 "Testperson" "Nachname") [Themenwahl (testthemen !! 0) 2]
  , SchuelerIn (Person 100001 "Testperson1" "Nachname") [Themenwahl (testthemen !! 0) 2]
  ]
  [ BetreuerIn (Person 200000 "Testbetreuer" "Nachname") testthemen
  ]
  testthemen
  testzeiteinheiten
  [ Raum (Node 200 "Raum") False ]

-- TODO ListT?
-- TODO Modularisieren

testLP :: Seminar -> LPM String Double ()
testLP seminar@(Seminar _ schuelerInnen betreuerInnen themen zeiteinheiten raeume) = do
  forM_ (moeglicheGlobalBelegungen seminar) $ \gb -> setVarKind (var gb) BinVar
  forM_ (moeglicheBetreuerBelegungen seminar) $ \bb -> do
    setVarKind (var bb) BinVar
    var bb `leq` var (bGlobalBelegung bb)
  sequence_ $ do
    betreuerIn <- betreuerInnen
    zeiteinheit <- zeiteinheiten
    return $ add (map var [BetreuerBelegung (GlobalBelegung thema zeiteinheit) | thema <- themen]) `leq` 1
  forM_ (moeglicheLokalBelegungen seminar) $ \lb -> do
    setVarKind (var lb) BinVar
    var lb `leq` var (lGlobalBelegung lb)
  sequence_ $ do
    schuelerIn <- schuelerInnen
    zeiteinheit <- zeiteinheiten
    return $ add [var (LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn) | thema <- themen] `leq` 1
    -- TODO Eigentlich wollen wir hier sowas wie "trace moeglicheGlobalBelegungen themen"


main :: IO ()
main = do
  seminar  <- leseSeminar "jena/"
  print seminar
