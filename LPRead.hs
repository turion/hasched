module LPRead where

import Data.Maybe (fromMaybe)

import Data.Map

import LPUtils
import Stundenplan

data LPParseError v
  = InvalidDouble Double
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
    parseList ((_, d) : _) = Left $ InvalidDouble d

-- TODO Lokal
parseStundenplan :: Seminar -> String -> Map String Double -> Either (LPParseError String) GlobalStundenplan
parseStundenplan seminar version lpResult = do
  globalBelegung <- stupidParse (moeglicheGlobalBelegungen seminar) lpResult
  betreuerBelegung <- stupidParse (moeglicheBetreuerBelegungen seminar) lpResult
  raumBelegung <- stupidParse (moeglicheRaumBelegungen seminar) lpResult
  return $ GlobalStundenplan seminar globalBelegung betreuerBelegung raumBelegung version
