import Parser
import Stundenplan 

main = do
  seminar <- leseSeminar "jena/" 
  let betreuer=betreuerInnen seminar
  print (betreuer!!1)
  
