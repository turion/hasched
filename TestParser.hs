import Parser
import Stundenplan
import GHC.Exts (sortWith) 

main = do
  seminar <- leseSeminar "jena/" 
  print seminar
  
