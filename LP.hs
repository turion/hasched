{-# LANGUAGE FlexibleContexts #-}
module LP where

import Control.Monad
import Prelude hiding ((-), (+))

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
    asLinFunc "minimalspaß" `leq` linCombination -- TODO Kann man das mit gesamtspass refactoren?
      [ ( praeferenz themenwahl
        , var $ LokalBelegung (GlobalBelegung (gewaehltesThema themenwahl) zeiteinheit) schuelerIn)
        | themenwahl <- themenwahlen schuelerIn
        , zeiteinheit <- zeiteinheiten seminar
      ]

-- | Globale Zwangsbedingungen werden hier definiert
global :: LPSeminarFun
global seminar = do
  implementiereMinimum seminar
  -- Ein Thema kann nur stattfinden, wenn BetreuerInnen dafür eingeteilt werden
  themaNurMitBetreuer seminar
  -- BetreuerInnen werden nur eingeteilt, wenn das Thema stattfindet
  betreuerNurFallsThemaStattfindet seminar  
  -- BetreuerInnen können zu einer Zeit höchstens an einem Ort sein
  betreurKoennenSichNichtSpalten seminar
  -- TODO Raumzuordnungen

  ausnahmeMussStattfindenAn seminar

  -- TODO Bedingungen für Nuklearexkursion


themaNurMitBetreuer :: LPSeminarFun
themaNurMitBetreuer seminar = 
  forM_ (moeglicheGlobalBelegungen seminar) $ \gb -> do
    setVarKind (var gb) BinVar
    varLF gb `leq` add
      [ varLF $ BetreuerBelegung gb betreuerIn
        | betreuerIn <- betreuerInnen seminar
      ]

betreuerNurFallsThemaStattfindet :: LPSeminarFun
betreuerNurFallsThemaStattfindet seminar  = 
  forM_ (moeglicheBetreuerBelegungen seminar) $ \bb -> do
    setVarKind (var bb) BinVar
    varLF bb `leq` varLF (bGlobalBelegung bb)

ausnahmeMussStattfindenAn :: LPSeminarFun
ausnahmeMussStattfindenAn seminar = sequence_ $ do
  thema <- themen seminar
  zeiteinheit <- mussStattfindenAn thema
  return $ varLF (GlobalBelegung thema zeiteinheit) `equalTo` 1

betreurKoennenSichNichtSpalten :: LPSeminarFun
betreurKoennenSichNichtSpalten seminar =
  sequence_ $ do
    betreuerIn <- betreuerInnen seminar
    zeiteinheit <- zeiteinheiten seminar
    return $ add
      [ varLF $ BetreuerBelegung (GlobalBelegung thema zeiteinheit) betreuerIn
        | thema <- themen seminar
      ] `leqTo` 1

 
raumPlanung :: LPSeminarFun
raumPlanung seminar = do
  -- Ein Raum wird nur belegt, wenn dort etwas stattfindet
  raumNichtUnnoetigBelegen seminar
  -- Für jedes Thema muss ein Raum gebucht sein
  themaMussRaumHaben seminar
  -- TODO Raumgrößen, Raumausnahmen und weitere Ausnahmen
  -- In einem Raum kann zu einer Zeit höchstens ein Thema stattfinden
  raumNichtDoppeltBelegen seminar

raumNichtUnnoetigBelegen :: LPSeminarFun
raumNichtUnnoetigBelegen seminar =
 forM_ (moeglicheRaumBelegungen seminar) $ \rb -> do
    setVarKind (var rb) BinVar
    varLF rb `leq` varLF (rGlobalBelegung rb)

themaMussRaumHaben :: LPSeminarFun 
themaMussRaumHaben seminar =
  sequence_ $ do
    thema <- themen seminar
    zeiteinheit <- zeiteinheiten seminar
    let gb = GlobalBelegung thema zeiteinheit
    return $ varLF gb `leq` add
      [ varLF $ RaumBelegung gb raum
        | raum <- raeume seminar
      ]

raumNichtDoppeltBelegen :: LPSeminarFun
raumNichtDoppeltBelegen seminar =
 sequence_ $ do
    raum <- raeume seminar
    zeiteinheit <- zeiteinheiten seminar
    return $ add
      [ varLF $ RaumBelegung (GlobalBelegung thema zeiteinheit) raum
        | thema <- themen seminar
      ] `leqTo` 1

-- Lokale (SchülerInnen betreffende) Zwangsbedingungen
lokal :: LPSeminarFun
lokal seminar = do
  -- SchülerInnen werden nur eingeteilt, wenn das Thema dann stattfindet
  schuelerInnenNichtUnnoetigEinteilen seminar --TODO: Diese Bedingung ist zu stark!!   -- SchülerInnen können zu einer Zeit höchstens an einem Ort sein
  schuelerInnenKoenneSichNichtSpalten seminar 
  -- Vorraussetzungen fuer ein gewaehltes thema muss der/die SchuelerIn schon belegt haben oder er kennt sie schon
  voraussetzungenErzwingen seminar
    -- TODO Eigentlich wollen wir hier sowas wie "trace moeglicheGlobalBelegungen themen"
  -- Jedes Thema wird höchstens einmal belegt
  themenNichtDoppeltBelegen seminar

schuelerInnenNichtUnnoetigEinteilen :: LPSeminarFun
schuelerInnenNichtUnnoetigEinteilen seminar = 
  forM_ (moeglicheLokalBelegungen seminar) $ \lb -> do
    setVarKind (var lb) BinVar
    varLF lb `leq` varLF (lGlobalBelegung lb)

schuelerInnenKoenneSichNichtSpalten :: LPSeminarFun
schuelerInnenKoenneSichNichtSpalten seminar =
 sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    zeiteinheit <- zeiteinheiten seminar
    return $ add
      [ varLF $ LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn
        | thema <- themen seminar
      ] `leqTo` 1

themenNichtDoppeltBelegen :: LPSeminarFun
themenNichtDoppeltBelegen seminar = 
  sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    thema <- themen seminar
    return $ add
      [ varLF $ LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn
        | zeiteinheit <- zeiteinheiten seminar
      ] `leqTo` 1

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
