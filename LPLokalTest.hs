import Parser
import Stundenplan
import LatexWriter
import LPLokal
import LPRead

import Data.Map

import Data.LinearProgram.GLPK

main = do
  seminar <- leseSeminar "muenchen2/"
  
  let neueBetreuer = [ BetreuerIn (Person (-1) "Ismail" "Achmed-Zade" []) [] ,BetreuerIn (Person (-2) "Samuel" "Moll" []) [] ,BetreuerIn (Person (-3) "Martin" "Großhauser" []) []]
  let seminar' = seminar {betreuerInnen = (betreuerInnen seminar) ++ neueBetreuer}
   
  global <- leseGlobalenPlan seminar' "extern/einheiten.xml"
  lpBerechnung <- glpSolveVars orpheusLPOptionen $ generiereLokalenPlan global
  case lpBerechnung of
    (retCode, Nothing)   -> putStrLn $ "Fehlgeschlagen: " ++ show retCode
    (_, Just (obj, lpResult)) -> do
      let lokal = parseLokal global lpResult
      makeLatex lokal
