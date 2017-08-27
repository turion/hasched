import Stundenplan
import LP
import Parser (leseSeminar)

import Data.LinearProgram.GLPK

import Text.Read (readMaybe)

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
main = do
  putStrLn "Teste mit 0) Jena-Testdaten 1) Testseminar (default)"
  auswahl <- readMaybe <$> getLine
  let
    getSeminar = case auswahl of
      Just 0  -> leseSeminar "jena/"
      Just 1  -> return testseminar
      Nothing -> do
        putStrLn "Default zu Testseminar"
        return testseminar
  seminar <- getSeminar
  stundenplan <- glpSolveVars mipDefaults $ testLP seminar
  print stundenplan
