module Main where

import Control.Monad

import Parser
import Stundenplan
import LP

main :: IO ()
main = do
  seminar  <- leseSeminar "muenchen1/"
  putStrLn $ concat $ zeigeHauef seminar
  --schreibeThemenZuBetreuer seminar
  --schreibeBetreuerZuThemen seminar
  
schreibeThemenZuBetreuer :: Seminar -> IO()
schreibeThemenZuBetreuer seminar = do
  forM_ (betreuerInnen seminar) $ \betr -> do
    let themenstring = themenZuString (betreuteThemen betr)
    putStrLn $ (vorname (bPerson betr))++" "++(nachname (bPerson betr))++": "++themenstring++"\n"

    
themenZuString :: [Themenwahl] -> String
themenZuString wahlen = concat $ map themaZuString wahlen

themaZuString :: Themenwahl -> String
themaZuString (Themenwahl thema preaf) = (titel (tnode thema)) ++ ": "++ (show preaf)++" "

schreibeBetreuerZuThemen :: Seminar -> IO()
schreibeBetreuerZuThemen seminar = do
  forM_ (themen seminar) $ \thema -> do
    let betreuerstring = betreuerZuString seminar thema
    putStrLn $ (titel (tnode thema)) ++ ": " ++ betreuerstring ++"\n"
    
betreuerZuString :: Seminar -> Thema -> String
betreuerZuString seminar thema = concat $ map (\(betr,p)->(vorname (bPerson betr))++" "++(nachname (bPerson betr))++": "++(show p)++", ") (findeBewertungen seminar thema)

findeBewertungen :: Seminar -> Thema -> [(BetreuerIn, Double)]
findeBewertungen seminar thema = concat $ map (findeBewertung thema) (betreuerInnen seminar) 

findeBewertung :: Thema -> BetreuerIn -> [(BetreuerIn,Double)]
findeBewertung thema betr = 
  let preaf = [p | (Themenwahl t p) <- (betreuteThemen betr), (nid (tnode t))==(nid (tnode thema))]
  in map (\p -> (betr,p)) preaf
