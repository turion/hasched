import Parser
import Stundenplan 

main = do
  seminar <- leseSeminar "jena/" 
  let t=themen seminar
  print t
  
