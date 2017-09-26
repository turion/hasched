import LatexWriter (makeLatex)
import LP
import LP.Read
import Parser (leseSeminar)
import Stundenplan
import Testseminar

-- glpk-hs
import Data.LinearProgram.GLPK

-- base
import Data.Map (delete)
import Text.Read (readMaybe)

-- GenericPretty
import Text.PrettyPrint.GenericPretty


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
  putStrLn "Lese Seminar..."
  seminar <- getSeminar
  putStrLn "Löse Optimierungsproblem..."
  lpBerechnung <- glpSolveVars orpheusLPOptionen $ testLP seminar
  --print lpBerechnung
  case lpBerechnung of
    (retCode, Nothing)   -> putStrLn $ "Fehlgeschlagen: " ++ show retCode
    (_, Just (obj, lpResult)) -> do
      putStrLn "Erfolgreich! Parse..."
      let lpResult' = delete "minimalspaß" lpResult -- TODO Aaaaaah
      case parseStundenplan seminar "testversion" lpResult' of
        Left e            -> print e
        Right stundenplan -> do
          putStrLn "Schreibe Stundenplan..."
          writeFile "tempstundenplan.txt" $ pretty $ strip stundenplan
          putStrLn "(Global, Betreuer, Raum)"
          print ( length $ globalBelegungen $ globalStundenplan stundenplan
                , length $ betreuerBelegungen $ globalStundenplan stundenplan
                , length $ raumBelegungen $ globalStundenplan stundenplan
                )
          putStrLn "Erzeuge LaTeX..."
          makeLatex stundenplan
