import Parser
import Stundenplan
import GHC.Exts (sortWith) 

main = do
  seminar <- leseSeminar "muenchen2/"
  
  let neueBetreuer = [ BetreuerIn (Person (-1) "Ismail" "Achmed-Zade" []) [] ,BetreuerIn (Person (-2) "Samuel" "Moll" []) [] ,BetreuerIn (Person (-3) "Martin" "Großhauser" []) []]
  let seminar' = seminar {betreuerInnen = (betreuerInnen seminar) ++ neueBetreuer}
   
  global <- leseGlobalenPlan seminar' "extern/einheiten.xml"
  print  global
