{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Stundenplan where

import LPUtils

import Data.List
import Data.Maybe (fromMaybe)

data Node = Node
  { nid   :: Integer
  , titel :: String
  }
  deriving (Show, Eq, Ord)

data Thema = Thema
  { tnode :: Node
  , raum :: Maybe Raum
  , tbeamer :: Bool
  , mussStattfindenAn :: [ Zeiteinheit ]
  , voraussetzungen :: [ Thema ]
  }
  deriving (Show, Eq, Ord)

instance LPVar Thema String where
  var (Thema (Node nid _) _ _ _ _) = "thema " ++ show nid

data ZeiteinheitTyp = Physikeinheit | Exkursion | Anderes
  deriving (Show, Eq, Ord)

data Zeiteinheit = Zeiteinheit
  { znode :: Node
  , zTyp :: ZeiteinheitTyp
  , zeit :: String
  }
  deriving (Show, Eq, Ord)

instance LPVar Zeiteinheit String where
  var (Zeiteinheit (Node nid _) _ _) = "zeiteinheit " ++ show nid

-- TODO Zu testen
-- | Findet alle vorherigen Zeiteinheiten,
-- | unter der Annahme, dass sie chronologisch sortiert sind
vorherigeZeiteinheiten :: Zeiteinheit -> [Zeiteinheit] -> [Zeiteinheit]
vorherigeZeiteinheiten z zs = take (fromMaybe 1 (elemIndex z zs) - 1) zs


data Raum = Raum
  { rnode :: Node
  , raumgroesse :: Int
  , rbeamer :: Bool
  , nichtVerfuegbar :: [ Zeiteinheit ]
  }
  deriving (Show, Eq, Ord)

instance LPVar Raum String where
  var (Raum (Node nid _) _  _ _) = "raum " ++ show nid

data Themenwahl = Themenwahl
  { gewaehltesThema :: Thema
  , praeferenz      :: Double
  }
  deriving (Eq, Ord, Show)

data Person = Person
  { uid      :: Integer
  , vorname  :: String
  , nachname :: String
  , verpasst :: [Zeiteinheit]
  }
  deriving (Eq, Ord, Show)

data SchuelerIn = SchuelerIn
  { sPerson      :: Person
  , themenwahlen :: [ Themenwahl ]
  }
  deriving (Eq, Ord, Show)

instance LPVar SchuelerIn String where
  var (SchuelerIn (Person uid _ _ _) _) = "schuelerin " ++ show uid

data BetreuerIn = BetreuerIn
  { bPerson        :: Person
  , betreuteThemen :: [ Themenwahl ]
  }
  deriving (Eq, Ord, Show)

instance LPVar BetreuerIn String where
  var (BetreuerIn (Person uid _ _ _) _) = "betreuerin " ++ show uid

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
  var (GlobalBelegung gbZeiteinheit gbThema) = "globalbelegung " ++ intercalate " " [var gbZeiteinheit, var gbThema]


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
  var (BetreuerBelegung bGlobalBelegung bBetreuerIn) = "betreuerbelegung " ++ var bGlobalBelegung ++ " " ++ var bBetreuerIn


data RaumBelegung = RaumBelegung
  { rGlobalBelegung :: GlobalBelegung
  , rRaum           :: Raum
  }

instance LPVar RaumBelegung String where
  var (RaumBelegung rGlobalBelegung rRaum) = "raumbelegung " ++ var rGlobalBelegung ++ " " ++ var rRaum

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
  deriving (Eq, Ord, Show)

instance LPVar LokalBelegung String where
  var (LokalBelegung lGlobalBelegung lSchuelerIn) = "lokalbelegung " ++ var lGlobalBelegung ++ " " ++ var lSchuelerIn

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
  
data LokalStundenplan =LokalStundenplan
  { globalStundenplan :: GlobalStundenplan
  , lokaleBelegungen :: [ LokalBelegung ] 
  }
