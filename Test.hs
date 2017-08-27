import Stundenplan
import LP

import Data.LinearProgram.GLPK

testthemen :: [ Thema ]
testthemen = [ Thema (Node 23 "Mondflug") Nothing False [] []
             , Thema (Node 42 "Supernova")  Nothing False [] [testthemen !! 0]
             ]

testzeiteinheiten :: [ Zeiteinheit ]
testzeiteinheiten = [ Zeiteinheit (Node 100 "Z0")
                    , Zeiteinheit (Node 101 "Z1")
                    ]

testseminar :: Seminar
testseminar = Seminar
  (Node 0 "Testseminar")
  [ SchuelerIn (Person 100000 "Testperson" "Nachname") [Themenwahl (testthemen !! 0) 2]
  , SchuelerIn (Person 100001 "Testperson1" "Nachname") [Themenwahl (testthemen !! 0) 2]
  ]
  [ BetreuerIn (Person 200000 "Testbetreuer" "Nachname") [Themenwahl thema 100 | thema <- testthemen]
  ]
  testthemen
  testzeiteinheiten
  [ Raum (Node 200 "Raum") 100 False ]

main :: IO ()
main = print =<< glpSolveVars mipDefaults (testLP testseminar)
