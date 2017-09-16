{-# LANGUAGE FlexibleContexts #-}
module LP where

import Control.Monad
import Prelude hiding ((-))

import Control.Monad.LPMonad
import Data.LinearProgram hiding ((+),(*),(/))
import Data.LinearProgram.GLPK.Solver
import Data.Maybe

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
    optimumGlobal seminar
    global seminar
    --lokal seminar
    setzeVariablentypen seminar

-- | Der optimale Stundenplan wird hier definiert
optimum :: LPSeminarFun
optimum seminar = do
  setDirection Max
  setObjective $ gesamtspass seminar  -- + linCombination [(1000, "minimalspaß")]

gesamtspass seminar = linCombination $
    [ ( praeferenz themenwahl
      , var $ LokalBelegung (GlobalBelegung (gewaehltesThema themenwahl) zeiteinheit) schuelerIn)
      | schuelerIn <- schuelerInnen seminar
      , themenwahl <- themenwahlen schuelerIn
      , zeiteinheit <- physikeinheiten seminar
    ]
    
optimumGlobal :: LPSeminarFun
optimumGlobal seminar = do
  setDirection Max
  --setObjective $ hauefigkeitenGlobal seminar
  setObjective $ linCombination []
   
hauefigkeitenGlobal seminar = linCombination $
  [( summierePraeferenz seminar thema
   , var $ GlobalBelegung thema zeiteinheit)
   | thema <- themen seminar
   , zeiteinheit <- physikeinheiten seminar
  ]

summierePraeferenz :: Seminar -> Thema -> Double
summierePraeferenz seminar thema =
  let tw = concat $ map themenwahlen $ schuelerInnen seminar
  in sum [preaf | (Themenwahl t preaf) <- tw, (nid (tnode t))==(nid (tnode thema))]
  
gesamtPraeferenz :: Seminar -> Double
gesamtPraeferenz seminar = sum [summierePraeferenz seminar thema | thema <- themen seminar]

implementiereMinimum :: LPSeminarFun
implementiereMinimum seminar = forM_ (schuelerInnen seminar) $ \schuelerIn -> do
    asLinFunc "minimalspaß" `leq` linCombination -- TODO Kann man das mit gesamtspass refactoren?
      [ ( praeferenz themenwahl
        , var $ LokalBelegung (GlobalBelegung (gewaehltesThema themenwahl) zeiteinheit) schuelerIn)
        | themenwahl <- themenwahlen schuelerIn
        , zeiteinheit <- physikeinheiten seminar
      ]

-- | Globale Zwangsbedingungen werden hier definiert
global :: LPSeminarFun
global seminar = do
  --implementiereMinimum seminar
  -- Ein Thema kann nur stattfinden, wenn BetreuerInnen dafür eingeteilt werden
  themaNurMitBetreuer seminar
  -- BetreuerInnen werden nur eingeteilt, wenn das Thema stattfindet
  betreuerNurFallsThemaStattfindet seminar  
  -- BetreuerInnen können zu einer Zeit höchstens an einem Ort sein
  betreurKoennenSichNichtSpalten seminar
  -- TODO Raumzuordnungen
  
  jedesThemaSoOftWieGewuenscht seminar
  
  themaBetreuerPlanung 4 seminar

  ausnahmeMussStattfindenAn seminar
  
  ausnahmeMussStattfindenIn seminar

  -- TODO Bedingungen für Nuklearexkursion
  
  raumPlanung seminar

setzeVariablentypen :: LPSeminarFun
setzeVariablentypen seminar = do
  forM_ (moeglicheGlobalBelegungen seminar) $ \gb -> do
    setVarKind (var gb) BinVar
  forM_ (moeglicheBetreuerBelegungen seminar) $ \bb -> do
    setVarKind (var bb) BinVar
  forM_ (moeglicheRaumBelegungen seminar) $ \rb -> do
    setVarKind (var rb) BinVar
  sequence_ $ do
    betr <- betreuerInnen seminar
    thema <- themen seminar
    return $ setVarKind (var (BetreuerThemenZuordnung betr thema)) BinVar
  --forM_ (moeglicheLokalBelegungen seminar) $ \lb -> do
    --setVarKind (var lb) BinVar

themaNurMitBetreuer :: LPSeminarFun
themaNurMitBetreuer seminar = 
  forM_ (moeglicheGlobalBelegungen seminar) $ \gb -> do
    varLF gb `leq` add
      [ varLF $ BetreuerBelegung gb betreuerIn
        | betreuerIn <- betreuerInnen seminar
      ]

betreuerNurFallsThemaStattfindet :: LPSeminarFun
betreuerNurFallsThemaStattfindet seminar  = 
  forM_ (moeglicheBetreuerBelegungen seminar) $ \bb -> do
    varLF bb `leq` varLF (bGlobalBelegung bb)

ausnahmeMussStattfindenAn :: LPSeminarFun
ausnahmeMussStattfindenAn seminar = sequence_ $ do
  thema <- themen seminar
  zeiteinheit <- mussStattfindenAn thema
  return $ varLF (GlobalBelegung thema zeiteinheit) `equalTo` 1

ausnahmeMussStattfindenIn :: LPSeminarFun
ausnahmeMussStattfindenIn seminar = sequence_ $ do
  thema <- themenMitRaum seminar
  zeiteinheit <- physikeinheiten seminar
  return $ (varLF (RaumBelegung (GlobalBelegung thema zeiteinheit) (fromJust (raum thema)))) `geq` (varLF (GlobalBelegung thema zeiteinheit))
  
themenMitRaum :: Seminar -> [Thema]
themenMitRaum seminar = filter (\t->(raum t)/=Nothing) $ themen seminar

betreurKoennenSichNichtSpalten :: LPSeminarFun
betreurKoennenSichNichtSpalten seminar =
  sequence_ $ do
    betreuerIn <- betreuerInnen seminar
    zeiteinheit <- physikeinheiten seminar
    return $ add
      [ varLF $ BetreuerBelegung (GlobalBelegung thema zeiteinheit) betreuerIn
        | thema <- themen seminar
      ] `leqTo` 1

jedesThemaMindestensEinMal :: LPSeminarFun
jedesThemaMindestensEinMal seminar =
  sequence_ $ do
    thema <- themen seminar
    return $ add
      [varLF $ GlobalBelegung thema zeiteinheit | zeiteinheit <- physikeinheiten seminar] `geqTo` 1
      
jedesThemaSoOftWieGewuenscht :: LPSeminarFun
jedesThemaSoOftWieGewuenscht seminar =
  forM_ (themen seminar) $ \thema -> do
    let preafCount = summierePraeferenz seminar thema
    let hauef =fromIntegral $ round $ preafCount/(gesamtPraeferenz seminar)*10*10
    let themenzahl = add [varLF $ GlobalBelegung thema zeiteinheit | zeiteinheit <- physikeinheiten seminar] 
    themenzahl `geqTo` (hauef -1)
    themenzahl `leqTo` (hauef + 1)
    --themenzahl `geqTo` 1
    --themenzahl `leqTo` 3
    
zeigeHauef :: Seminar -> [String]
zeigeHauef seminar = map (\t -> (titel (tnode t))++": "++ show (berechneHauefigkeit seminar t)++"\n") (themen seminar)

berechneHauefigkeit :: Seminar -> Thema -> Double
berechneHauefigkeit seminar thema =
  let preafCount = summierePraeferenz seminar thema
  in preafCount/(gesamtPraeferenz seminar)*7*10

themaBetreuerPlanung :: Int -> LPSeminarFun
themaBetreuerPlanung n seminar = do
  themaNurWennBetreuerZugeordnet seminar
  proThemaNurEinBetreuer seminar
  proBetreuerMaximalNThemen 4 seminar
    
themaNurWennBetreuerZugeordnet :: LPSeminarFun
themaNurWennBetreuerZugeordnet seminar = 
  sequence_ $ do
    thema <- themen seminar
    betreuerIn <- betreuerInnen seminar
    zeiteinheit <- zeiteinheiten seminar
    return $ (varLF (BetreuerBelegung (GlobalBelegung thema zeiteinheit) betreuerIn )) `leq` (varLF (BetreuerThemenZuordnung betreuerIn thema))
    
proThemaNurEinBetreuer :: LPSeminarFun
proThemaNurEinBetreuer seminar =
  forM_ (themen seminar) $ \thema -> do
    add [ varLF $ BetreuerThemenZuordnung betreuerIn thema | betreuerIn <- betreuerInnen seminar] `equalTo` 1
  
proBetreuerMaximalNThemen :: Double -> LPSeminarFun
proBetreuerMaximalNThemen n seminar =
   forM_ (betreuerInnen seminar) $ \betreuerIn -> do
    add [varLF $ BetreuerThemenZuordnung betreuerIn thema | thema <- themen seminar] `leqTo` n
 
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
    varLF rb `leq` varLF (rGlobalBelegung rb)

themaMussRaumHaben :: LPSeminarFun 
themaMussRaumHaben seminar =
  sequence_ $ do
    thema <- themen seminar
    zeiteinheit <- physikeinheiten seminar
    let gb = GlobalBelegung thema zeiteinheit
    return $ varLF gb `leq` add
      [ varLF $ RaumBelegung gb raum
        | raum <- raeume seminar
      ]

raumNichtDoppeltBelegen :: LPSeminarFun
raumNichtDoppeltBelegen seminar =
 sequence_ $ do
    raum <- raeume seminar
    zeiteinheit <- physikeinheiten seminar
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
  --voraussetzungenErzwingen seminar
    -- TODO Eigentlich wollen wir hier sowas wie "trace moeglicheGlobalBelegungen themen"
  -- Jedes Thema wird höchstens einmal belegt
  themenNichtDoppeltBelegen seminar

schuelerInnenNichtUnnoetigEinteilen :: LPSeminarFun
schuelerInnenNichtUnnoetigEinteilen seminar = 
  forM_ (moeglicheLokalBelegungen seminar) $ \lb -> do
    varLF lb `leq` varLF (lGlobalBelegung lb)

schuelerInnenKoenneSichNichtSpalten :: LPSeminarFun
schuelerInnenKoenneSichNichtSpalten seminar =
 sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    zeiteinheit <- physikeinheiten seminar
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
        | zeiteinheit <- physikeinheiten seminar
      ] `leqTo` 1

voraussetzungenErzwingen :: LPSeminarFun
voraussetzungenErzwingen seminar = do
  -- Diese Variable gibt an,
  -- ob jemand das Thema zu einer bestimmten Zeit schon besucht hat,
  -- oder angegeben hat, dass er/sie sich damit auskennt (Präferenz 0 Sterne)
  sequence_ $ do
    schuelerIn <- schuelerInnen seminar
    thema <- themen seminar
    zeiteinheit <- physikeinheiten seminar
    let varKompetent = var ("kompetent", LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn)
    return $ do
      setVarKind varKompetent BinVar
      (asLinFunc varKompetent - add
          [ varLF (GlobalBelegung thema vorher)
            | vorher <- vorherigeZeiteinheiten zeiteinheit $ physikeinheiten seminar
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
    zeiteinheit <- physikeinheiten seminar
    voraussetzung <- voraussetzungen thema
    return $
      varLF (LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn)
      `leq` varLF ("kompetent", LokalBelegung (GlobalBelegung voraussetzung zeiteinheit) schuelerIn)
