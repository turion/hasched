{-# LANGUAGE FlexibleContexts #-}
module LPLokal where

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

type LPGlobalFun = GlobalStundenplan -> LPM String Double ()

generiereLokalenPlan :: GlobalStundenplan -> LP String Double
generiereLokalenPlan plan 
  = execLPM $ do
    optimum plan
    setzeVariablentypen plan
    zwangsbedingungen plan
    
    
optimum :: LPGlobalFun
optimum plan = do
  setDirection Max
  setObjective $ linCombination
    [(findePreaferenz schuelerIn thema
     , var $ LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn)
      | schuelerIn <- schuelerInnen (seminar plan)
      , zeiteinheit <- physikeinheiten (seminar plan)
      , thema <- themenVonZeiteinheit plan zeiteinheit
    ]
    
setzeVariablentypen :: LPGlobalFun
setzeVariablentypen plan = sequence_ $ do
  schuelerIn <- schuelerInnen (seminar plan)
  zeiteinheit <- physikeinheiten (seminar plan)
  thema <- themenVonZeiteinheit plan zeiteinheit
  return $ setVarKind (var (LokalBelegung (GlobalBelegung thema zeiteinheit) schuelerIn)) BinVar
  
zwangsbedingungen :: LPGlobalFun
zwangsbedingungen plan = do
  jederSchuelerBesuchtThemaHoechstensEinMal plan
  jederSchuelerBesuchtProZeiteinheitGenauEinThema plan
  raumgroesseMussEingehaltenWerden plan
  
jederSchuelerBesuchtThemaHoechstensEinMal :: LPGlobalFun
jederSchuelerBesuchtThemaHoechstensEinMal plan = sequence_ $ do
  thema <- themen (seminar plan)
  schueler <- schuelerInnen (seminar plan)
  return $ add 
    [ varLF (LokalBelegung (GlobalBelegung thema zeiteinheit) schueler) | zeiteinheit <- zeiteinheitenVonThema plan thema] `leqTo` 1  
 
jederSchuelerBesuchtProZeiteinheitGenauEinThema :: LPGlobalFun
jederSchuelerBesuchtProZeiteinheitGenauEinThema plan = sequence_ $ do
  schueler <- schuelerInnen (seminar plan)
  zeiteinheit <- physikeinheiten (seminar plan)
  return $ add
    [varLF (LokalBelegung (GlobalBelegung thema zeiteinheit) schueler) | thema <- themenVonZeiteinheit plan zeiteinheit] `equalTo` 1
    
raumgroesseMussEingehaltenWerden :: LPGlobalFun
raumgroesseMussEingehaltenWerden plan = sequence_ $ do
  zeiteinheit <- physikeinheiten (seminar plan)
  thema <- themenVonZeiteinheit plan zeiteinheit
  return $ add
    [varLF (LokalBelegung (GlobalBelegung thema zeiteinheit) schueler) | schueler <- schuelerInnen (seminar plan)] `leqTo` (fromIntegral (raumgroesse (raumVonEinheit plan zeiteinheit thema)))
   

findePreaferenz :: SchuelerIn -> Thema -> Double
findePreaferenz schueler thema =
  let preafs = [p | (Themenwahl t p) <- themenwahlen schueler, t==thema]
  in if (length preafs)==1 then head preafs else 0 
    
themenVonZeiteinheit :: GlobalStundenplan -> Zeiteinheit -> [Thema]
themenVonZeiteinheit plan ze = [thema | (GlobalBelegung thema z)<-globalBelegungen plan, z==ze]

zeiteinheitenVonThema :: GlobalStundenplan -> Thema -> [Zeiteinheit]
zeiteinheitenVonThema plan thema = [ze | (GlobalBelegung t ze)<-globalBelegungen plan, t==thema]

raumVonEinheit :: GlobalStundenplan -> Zeiteinheit -> Thema -> Raum
raumVonEinheit plan ze thema = 
  let filt = [raum | (RaumBelegung gb raum) <- raumBelegungen plan, gb == (GlobalBelegung thema ze)]
  in if (length filt)==1 then head filt else error ("Zur Zeiteinheit "++(titel (znode ze))++" findet Thema "++(titel (tnode thema))++" nicht statt")
  
betreuerVonEinheit :: GlobalStundenplan -> Zeiteinheit -> Thema -> BetreuerIn
betreuerVonEinheit plan ze thema = 
  let filt = [betreuer | (BetreuerBelegung gb betreuer) <- betreuerBelegungen plan, gb == (GlobalBelegung thema ze)]
  in if (length filt)==1 then head filt else error ("Zur Zeiteinheit "++(titel (znode ze))++" findet Thema "++(titel (tnode thema))++" nicht statt")




