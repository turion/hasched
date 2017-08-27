import Parser
import Stundenplan
import GHC.Exts (sortWith) 

main = do
  seminar <- leseSeminar "jena/" 
  let z= zeiteinheiten seminar
  print z
  
