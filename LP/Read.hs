module LP.Read where

import Data.Maybe (fromMaybe)

import Data.Map

import LP.Utils
import Stundenplan

data LPParseError v
  = InvalidDouble v Double
  | InexistentVar v
  deriving Show


stupidParse :: LPVar a v => [a] -> Map v Double -> Either (LPParseError v) [a]
stupidParse as lpResult = parseList $ assocs lpResult
  where
    parseList [] = Right []
    parseList ((_, 0) : vds) = parseList vds
    parseList ((v, 1) : vds) = do
      rest <- parseList vds
      -- maybe (Left (InexistentVar v)) (return . (: rest)) $ val as v
      return $ maybe rest (: rest) $ val as v
    parseList ((v, d) : _) = Left $ InvalidDouble v d

-- TODO Lokal
parseStundenplan :: Seminar -> String -> Map String Double -> Either (LPParseError String) LokalStundenplan
parseStundenplan seminar version lpResult = do
  globalBelegung <- stupidParse (moeglicheGlobalBelegungen seminar) lpResult
  betreuerBelegung <- stupidParse (moeglicheBetreuerBelegungen seminar) lpResult
  raumBelegung <- stupidParse (moeglicheRaumBelegungen seminar) lpResult
  lokalBelegungen <- stupidParse (moeglicheLokalBelegungen seminar) lpResult
  let globalplan = GlobalStundenplan seminar globalBelegung betreuerBelegung raumBelegung version
  return $ LokalStundenplan globalplan lokalBelegungen
