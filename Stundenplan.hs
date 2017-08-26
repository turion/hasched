{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Stundenplan where

import LPUtils
import Data.List

data Node = Node
  { nid   :: Integer
  , titel :: String
  }
  deriving (Show, Eq, Ord)

data Thema = Thema
  { tnode :: Node
  , raum :: Maybe Raum
  , tbeamer :: Bool
  , voraussetzungen :: [ Thema ]
  }
  deriving (Show, Eq, Ord)

instance LPVar Thema String where
  var (Thema (Node nid _) _ _ _) = "thema " ++ show nid

data Zeiteinheit = Zeiteinheit
  { znode :: Node
  }
  deriving (Show, Eq, Ord)

instance LPVar Zeiteinheit String where
  var (Zeiteinheit (Node nid _)) = "zeiteinheit " ++ show nid


data Raum = Raum
  { rnode :: Node
  , raumgroesse :: Int 
  , rbeamer :: Bool
  }
  deriving (Show, Eq, Ord)

instance LPVar Raum String where
  var (Raum (Node nid _) _  _) = "raum " ++ show nid

data Themenwahl = Themenwahl
  { gewaehltesThema :: Thema
  , praeferenz      :: Double
  }
  deriving (Eq, Ord, Show)

data Person = Person
  { uid      :: Integer
  , vorname  :: String
  , nachname :: String
  }
  deriving (Eq, Ord, Show)

data SchuelerIn = SchuelerIn
  { sPerson      :: Person
  , themenwahlen :: [ Themenwahl ]
  }
  deriving (Eq, Ord, Show)

instance LPVar SchuelerIn String where
  var (SchuelerIn (Person uid _ _) _) = "schuelerin " ++ show uid

data BetreuerIn = BetreuerIn
  { bPerson        :: Person
  , betreuteThemen :: [ Thema ]
  }
  deriving (Eq, Ord, Show)
  
instance LPVar BetreuerIn String where
  var (BetreuerIn (Person uid _ _) _) = "betreuerin " ++ show uid

data Seminar = Seminar
  { semnode       :: Node
  , schuelerInnen :: [ SchuelerIn ]
  , betreuerInnen :: [ BetreuerIn ]
  , themen        :: [ Thema ]
  , zeiteinheiten :: [ Zeiteinheit ]
  , raeume        :: [ Raum ]
  }
  deriving(Show)

data GlobalBelegung = GlobalBelegung
  { gbThema       :: Thema
  , gbZeiteinheit :: Zeiteinheit
  }
  deriving (Eq, Ord, Show)


-- TODO ReaderT Seminar?
moeglicheGlobalBelegungen :: Seminar -> [ GlobalBelegung ]
moeglicheGlobalBelegungen (Seminar _ _ _ themen zeiteinheiten raeume)
  = [ GlobalBelegung t z
    | t <- themen
    , z <- zeiteinheiten
    ]

instance LPVar GlobalBelegung String where
  var (GlobalBelegung gbZeiteinheit gbThema) = intercalate " " [var gbZeiteinheit, var gbThema]


data BetreuerBelegung = BetreuerBelegung
  { bGlobalBelegung :: GlobalBelegung
  , bBetreuerIn     :: BetreuerIn
  , bRaum           :: Raum
  }

moeglicheBetreuerBelegungen :: Seminar -> [ BetreuerBelegung ]
moeglicheBetreuerBelegungen seminar
  = [ BetreuerBelegung gb b r
    | gb <- moeglicheGlobalBelegungen seminar
    , b  <- betreuerInnen seminar
    , r  <- raeume seminar
    ]
instance LPVar BetreuerBelegung String where
  var (BetreuerBelegung bGlobalBelegung bBetreuerIn _) = var bGlobalBelegung ++ " " ++ var bBetreuerIn

data LokalBelegung = LokalBelegung
  { lbGlobalBelegung :: GlobalBelegung
  , lbSchuelerIn   :: SchuelerIn
  }
  deriving (Eq, Ord, Show)

moeglicheLokalBelegungen :: Seminar -> [ LokalBelegung ]
moeglicheLokalBelegungen seminar
  = [ LokalBelegung gb s
      | gb <- moeglicheGlobalBelegungen seminar
      , s  <- schuelerInnen seminar
    ]

data GlobalStundenplan = GlobalStundenplan
  { seminar            :: Seminar
  , globalBelegungen   :: [ GlobalBelegung ]
  , betreuerBelegungen :: [ BetreuerBelegung ]
  , version            :: String
  }
