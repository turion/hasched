import Parser
import Stundenplan
import GHC.Exts (sortWith) 

main = do
  seminar <- leseSeminar "jena/" 
  let s= schuelerInnen seminar
  print (head s)
  
