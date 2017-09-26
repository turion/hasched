import Parser
import Stundenplan
import LatexWriter
import LPLokal
import LPRead

import Data.Map

import Data.LinearProgram.GLPK

main = do
  seminar <- leseSeminar "muenchen2/"
  
  let neueBetreuer = [ BetreuerIn (Person (-1) "Ismail" "Achmed-Zade" []) [] ,BetreuerIn (Person (-2) "Samuel" "Moll" []) [] ,BetreuerIn (Person (-3) "Martin" "Großhauser" []) [], BetreuerIn (Person (-4) "Aaron" "Wild" []) []]
  let seminar' = seminar {betreuerInnen = (betreuerInnen seminar) ++ neueBetreuer}
  --print $ themenwahlen $ head $ schuelerInnen seminar
   
  global <- leseGlobalenPlan seminar' "extern/einheiten.xml"
  zuweisungen <- leseExkursionsZwangsbedingungen seminar' "extern/zuordnungen.xml"
  --let zuweisungen = []
  --print $ getLinFun global
  --let thema = head $ Prelude.filter (\t->(nid (tnode t))== 199) (themen seminar)
  --print $ findePreaferenz (head (schuelerInnen seminar)) thema
  lpBerechnung <- glpSolveVars orpheusLPOptionen $ generiereLokalenPlan global zuweisungen
  case lpBerechnung of
    (retCode, Nothing)   -> putStrLn $ "Fehlgeschlagen: " ++ show retCode
    (_, Just (obj, lpResult)) -> do
      let lokal = parseLokal global lpResult
      makeLatex lokal
