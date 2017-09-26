{-# LANGUAGE FlexibleContexts #-}
module LP where

-- base
import Control.Monad
import Prelude hiding ((-), (+))

-- hasched
import LP.Imports

import LP.Betreuer
import LP.Exkursionen
import LP.Lokal
import LP.Raum

orpheusLPOptionen :: GLPOpts
orpheusLPOptionen = mipDefaults
  { tmLim = 1000 -- Nach 1000 Sekunden
  , mipGap = 0.1 -- 10 % Abstand zum Optimum sind ausreichend
  }

-- TODO Überall mit Record Wildcards drübergehen


-- TODO ListT?
testLP :: Seminar -> LP String Double
testLP seminar
  = execLPM $ do
    optimum seminar
    global seminar
    lokal seminar
    setzeVariablentypen seminar

-- | Der optimale Stundenplan wird hier definiert
optimum :: LPSeminarFun
optimum seminar = do
  setDirection Max
  setObjective $ gesamtspass seminar + linCombination [(1000, "minimalspaß")]

gesamtspass seminar = linCombination $
    [ ( praeferenz themenwahl
      , var $ LokalBelegung (GlobalBelegung (gewaehltesThema themenwahl) zeiteinheit) schuelerIn)
      | schuelerIn <- schuelerInnen seminar
      , themenwahl <- themenwahlen schuelerIn
      , zeiteinheit <- zeiteinheiten seminar
    ]


implementiereMinimum :: LPSeminarFun
implementiereMinimum seminar = forM_ (schuelerInnen seminar) $ \schuelerIn -> do
    asLinFunc "minimalspaß" `leq` add
      [ linCombination
        [ ( praeferenz themenwahl
          , var $ LokalBelegung (GlobalBelegung (gewaehltesThema themenwahl) zeiteinheit) schuelerIn)
          | themenwahl <- themenwahlen schuelerIn
        ]
        | zeiteinheit <- zeiteinheiten seminar
      ]

-- | Globale Zwangsbedingungen werden hier definiert
global :: LPSeminarFun
global seminar = do
  implementiereMinimum seminar
  globalNotwendigkeiten seminar
  globalAusnahmen seminar
  betreuerInnenZuordnungen seminar
  -- TODO Betreuer zu Themen zuordnen

-- | Globale Zwangsbedingungen, damit ein überhaupt sinnvoller und physisch möglicher Stundenplan rauskommt
globalNotwendigkeiten :: LPSeminarFun
globalNotwendigkeiten seminar = do
  betreuerNotwendigkeiten seminar
  raumNotwendigkeiten seminar
  exkursionen seminar


-- | Globale Zwangsbedingungen für Ausnahmen
globalAusnahmen :: LPSeminarFun
globalAusnahmen seminar = do
  raumAusnahmen seminar
  themaMussStattfindenAn seminar
  betreuerInnenVerpassen seminar


  -- TODO Raumzuordnungen


  -- TODO Bedingungen für Nuklearexkursion

-- TODO Exkursionen überhaupt

-- TODO Sind dabei u.U. andere Variablentypen verloren gegangen?
setzeVariablentypen :: LPSeminarFun
setzeVariablentypen seminar = do
  forM_ (moeglicheGlobalBelegungen seminar) $ \gb -> do
    setVarKind (var gb) BinVar
  forM_ (moeglicheBetreuerBelegungen seminar) $ \bb -> do
    setVarKind (var bb) BinVar
  forM_ (moeglicheRaumBelegungen seminar) $ \rb -> do
    setVarKind (var rb) BinVar
  forM_ (moeglicheLokalBelegungen seminar) $ \lb -> do
    setVarKind (var lb) BinVar

themaMussStattfindenAn :: LPSeminarFun
themaMussStattfindenAn seminar = sequence_ $ do
  thema <- themen seminar
  zeiteinheit <- mussStattfindenAn thema
  return $ varLF (GlobalBelegung thema zeiteinheit) `equalTo` 1
