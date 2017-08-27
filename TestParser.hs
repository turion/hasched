import Parser
import Stundenplan
import GHC.Exts (sortWith) 

main = do
  seminar <- leseSeminar "jena/" 
  let r= sortWith (nid.rnode) (raeume seminar)
  print r
  
