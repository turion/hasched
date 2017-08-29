import LatexWriter
import Stundenplan

main=do
  glob <- readFile "tempstundenplan.txt"
  makeLatex $ LokalStundenplan (read glob) []
    
testthemen :: [ Thema ]
testthemen = [ Thema (Node 23 "Mondflug") Nothing False [] []
             , Thema (Node 42 "Supernova")  Nothing False [] [testthemen !! 0]
             ]

testzeiteinheiten :: [ Zeiteinheit ]
testzeiteinheiten = [ Zeiteinheit (Node 100 "Z0") Physikeinheit "dann"
                    , Zeiteinheit (Node 101 "Z1") Physikeinheit "und wann"
                    ]
                    
betreuer=BetreuerIn (Person 200000 "Testbetreuer" "Nachname" []) [Themenwahl thema 100 | thema <- testthemen]

testraum=Raum (Node 200 "Testraum") 100 False []

testseminar :: Seminar
testseminar = Seminar
  (Node 0 "Testseminar")
  [ SchuelerIn (Person 100000 "Testperson" "Nachname" []) [Themenwahl (testthemen !! 0) 2]
  , SchuelerIn (Person 100001 "Testperson1" "Nachname" []) [Themenwahl (testthemen !! 0) 2]
  ]
  [betreuer]
  testthemen
  testzeiteinheiten
  [ testraum ]
  
globalB = [ GlobalBelegung (testthemen!!0) (testzeiteinheiten!!0),GlobalBelegung (testthemen!!1) (testzeiteinheiten!!1)]

betreuerB= [ BetreuerBelegung (globalB!!0) betreuer, BetreuerBelegung (globalB!!1) betreuer]

raumB = [ RaumBelegung (globalB!!0) testraum, RaumBelegung (globalB!!1) testraum]

global =GlobalStundenplan testseminar globalB betreuerB raumB "0.1"

lokal =LokalStundenplan global []


  
