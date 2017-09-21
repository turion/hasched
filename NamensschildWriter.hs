module NamensschildWriter where

import GHC.Exts (sortWith)
import Data.List.Split
import Stundenplan

positionen :: [(Double, Double)]
positionen =[(-4.5,11.2),(-4.5,5.6),(-4.5,0.0),(-4.5,-5.6),(-4.5,-11.2),(4.5,11.2),(4.5,5.6),(4.5,0.0),(4.5,-5.6),(4.5,-11.2)]

side :: String
side ="\\begin{tikzpicture}[remember picture, overlay, decoration={random steps, segment length=1mm, amplitude=.5mm}]\n"

schreibeNamensschilder :: Seminar -> IO ()
schreibeNamensschilder seminar = do
  prefix <- readFile "vorlagen/NamensschildPrefix.tex"
  writeFile "out/Namensschilder.tex" $ schreibeNamensschilderZuString seminar prefix


schreibeNamensschilderZuString :: Seminar -> String -> String
schreibeNamensschilderZuString seminar prefix =
  let personenAufSeite = sortierePersonen seminar
  in prefix ++ (concat (map schreibeSeite personenAufSeite)) ++"\n \\end{document}"
  
sortierePersonen :: Seminar -> [[Person]]
sortierePersonen seminar = chunksOf 10 $
  (sortWith vorname (map sPerson (schuelerInnen seminar)))
  ++ (sortWith vorname (map bPerson (betreuerInnen seminar)))
  
schreibeSeite :: [Person] -> String
schreibeSeite personen = 
  side ++ 
  (vorderseite 0 personen) ++
  "\\end{tikzpicture}\\newpage\n" ++
  side ++
  (rueckseite 0) ++
  "\\end{tikzpicture}\\newpage\n"
  
vorderseite ::Int -> [Person] -> String
vorderseite  10 _ =""
vorderseite _ [] = ""
vorderseite n (p:ps) =(vorderseite' (positionen !! n) p)++(vorderseite (n+1) ps)

vorderseite' :: (Double,Double) -> Person -> String
vorderseite' (x,y) p = 
  "\\Namensschild{"++
  (show x)++
  "cm}{" ++
  (show y)++
  "cm}{"++
  (vorname p) ++" "++(nachname p)++
  "}{Ort}\n"
  
rueckseite :: Int -> String
rueckseite 10 =""
rueckseite n = (rueckseite' (positionen !! n)) ++ (rueckseite (n+1))

rueckseite' :: (Double,Double) -> String
rueckseite' (x,y)=
  "\\Rueckseite{"++
  (show x) ++
  "cm}{"++
  (show y) ++
  "cm}\n"
