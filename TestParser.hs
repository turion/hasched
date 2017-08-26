import Parser
import Stundenplan 

main = do
  seminar <- leseSeminar "jena/" 
  let t = head $ themen seminar 
  voraussetzungen <- leseVoraussetzungen "jena/"
  print voraussetzungen 
