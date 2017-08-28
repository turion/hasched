import Stundenplan
import LP
import LPRead
import Parser (leseSeminar)

import Data.LinearProgram.GLPK

import Text.Read (readMaybe)

testthemen :: [ Thema ]
testthemen = [ Thema (Node 23 "Mondflug") Nothing False [] []
             , Thema (Node 42 "Supernova")  Nothing False [] [testthemen !! 0]
             ]

testzeiteinheiten :: [ Zeiteinheit ]
testzeiteinheiten = [ Zeiteinheit (Node 100 "Z0") Physikeinheit "dann"
                    , Zeiteinheit (Node 101 "Z1") Physikeinheit "und wann"
                    ]

testseminar :: Seminar
testseminar = Seminar
  (Node 0 "Testseminar")
  [ SchuelerIn (Person 100000 "Testperson" "Nachname" []) [Themenwahl (testthemen !! 0) 2]
  , SchuelerIn (Person 100001 "Testperson1" "Nachname" []) [Themenwahl (testthemen !! 0) 2]
  ]
  [ BetreuerIn (Person 200000 "Testbetreuer" "Nachname" []) [Themenwahl thema 100 | thema <- testthemen]
  ]
  testthemen
  testzeiteinheiten
  [ Raum (Node 200 "Raum") 100 False [] ]

main :: IO ()
main = do
  putStrLn "Teste mit 0) Testseminar (default) 1) Jena-Testdaten 2) Jena-Testdaten mit einem Drittel der SchülerInnen"
  auswahl <- readMaybe <$> getLine
  let
    getSeminar = case auswahl of
      Just 0 -> return testseminar
      Just 1 -> leseSeminar "jena/"
      Just 2 -> do
        seminar <- leseSeminar "jena/"
        let alleSchuelerInnen = schuelerInnen seminar
        return $ seminar { schuelerInnen = take (length alleSchuelerInnen `div` 3) alleSchuelerInnen }
      _      -> do
        putStrLn "Default zu Testseminar"
        return testseminar
  seminar <- getSeminar
  stundenplan <- glpSolveVars orpheusLPOptionen $ testLP seminar
  case stundenplan of
    (retCode, Nothing)   -> putStrLn $ "Fehlgeschlagen: " ++ show retCode
    (_, Just (obj, lpResult)) -> print $ parseStundenplan seminar "testversion" lpResult
