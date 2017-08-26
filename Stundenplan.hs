{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Stundenplan where

import LPUtils
import Data.List

data Node = Node
  { nid   :: Integer
  , titel :: String
  }
  deriving (Eq, Ord)

data Thema = Thema
  { tnode :: Node
  , voraussetzungen :: [ Thema ]
  }
  deriving (Eq, Ord)

instance LPVar Thema String where
  var (Thema (Node nid _) _) = "thema " ++ show nid

data Zeiteinheit = Zeiteinheit
  { znode :: Node
  }
  deriving (Eq, Ord)

instance LPVar Zeiteinheit String where
  var (Zeiteinheit (Node nid _)) = "zeiteinheit " ++ show nid


data Raum = Raum
  { rnode :: Node
  , beamer :: Bool
  }
  deriving (Eq, Ord)

instance LPVar Raum String where
  var (Raum (Node nid _) _) = "raum " ++ show nid

data Themenwahl = Themenwahl
  { gewaehltesThema :: Thema
  , praeferenz      :: Double
  }
  deriving (Eq, Ord)

data Person = Person
  { uid      :: Integer
  , vorname  :: String
  , nachname :: String
  }
  deriving (Eq, Ord)

data SchuelerIn = SchuelerIn
  { sPerson      :: Person
  , themenwahlen :: [ Themenwahl ]
  }
  deriving (Eq, Ord)

instance LPVar SchuelerIn String where
  var (SchuelerIn (Person uid _ _) _) = "schuelerin " ++ show uid

data BetreuerIn = BetreuerIn
  { bPerson        :: Person
  , betreuteThemen :: [ Thema ]
  }
  deriving (Eq, Ord)

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

data GlobalBelegung = GlobalBelegung
  { gbThema       :: Thema
  , gbZeiteinheit :: Zeiteinheit
  }
  deriving (Eq, Ord)


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
  }

moeglicheBetreuerBelegungen :: Seminar -> [ BetreuerBelegung ]
moeglicheBetreuerBelegungen seminar
  = [ BetreuerBelegung gb b
      | gb <- moeglicheGlobalBelegungen seminar
      , b  <- betreuerInnen seminar
    ]

instance LPVar BetreuerBelegung String where
  var (BetreuerBelegung bGlobalBelegung bBetreuerIn) = var bGlobalBelegung ++ " " ++ var bBetreuerIn


data RaumBelegung = RaumBelegung
  { rGlobalBelegung :: GlobalBelegung
  , bRaum           :: Raum
  }

moeglicheRaumBelegungen :: Seminar -> [ RaumBelegung ]
moeglicheRaumBelegungen seminar
  = [ RaumBelegung gb r
      | gb <- moeglicheGlobalBelegungen seminar
      , r  <- raeume seminar
    ]

data LokalBelegung = LokalBelegung
  { lGlobalBelegung :: GlobalBelegung
  , lSchuelerIn     :: SchuelerIn
  }
  deriving (Eq, Ord)

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
  , raumBelegungen     :: [ RaumBelegung ]
  , version            :: String
  }