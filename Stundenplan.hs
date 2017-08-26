module Stundenplan where

data Node = Node
  { nid   :: Integer
  , titel :: Node
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
  var (Zeiteinheit (Node nid _) _) = "zeiteinheit " ++ show nid


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

data BetreuerIn = BetreuerIn
  { bPerson        :: Person
  , betreuteThemen :: [ Thema ]
  }
  deriving (Eq, Ord)

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
  = [ GlobalBelegung z t
    | t <- themen
    , z <- zeiteinheiten
    ]

instance LPVar GlobalBelegung String where
  var (GlobalBelegung gbZeiteinheit gbRaum gbThema) = intercalate " " [var gbZeiteinheit, var gbRaum, var gbThema]


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
    , r  <- raeume
    ]
instance LPVar BetreuerBelegung String where
  var (BetreuerBelegung bGlobalBelegung bBetreuerIn) = var bGlobalBelegung ++ " " ++ var bBetreuerIn

instance LPVar BetreuerBelegung String where
  var (Raum (Node nid _) _) = "raum " ++ show nid

data LokalBelegung = LokalBelegung
  { lbGlobalBelegung :: GlobalBelegung
  , lbSchuelerIn   :: SchuelerIn
  }
  deriving (Eq, Ord)

moeglicheLokalBelegungen :: Seminar -> [ LokalBelegung ]
moeglicheLokalBelegungen seminar
  = [ LokalBelegung gb b
      | gb <- moeglicheGlobalBelegungen seminar
      , s  <- schuelerInnen seminar
    ]

data GlobalStundenplan = GlobalStundenplan
  { seminar            :: Seminar
  , globalBelegungen   :: [ GlobalBelegung ]
  , betreuerBelegungen :: [ BetreuerBelegungen ]
  , version            :: String
  }
