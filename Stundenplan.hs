{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stundenplan where

import LPUtils

import Control.Arrow (first)
import Data.List
import Data.Maybe (fromMaybe)

newtype Nid = Nid Integer
  deriving (Ord, Eq, Num)

-- Konstruktor muss bei read umgangen werden
instance Read Nid where
  readsPrec prec = map (first Nid) . readsPrec prec
  
instance Show Nid where
  show (Nid i) = show i

newtype Uid = Uid Integer
  deriving (Ord, Eq, Num)

instance Read Uid where
  readsPrec prec = map (first Uid) . readsPrec prec
  
instance Show Uid where
  show (Uid i) = show i

data Node = Node
  { nid   :: Nid
  , titel :: String
  }
  deriving (Show, Eq, Ord, Read)

unique :: [a] -> Maybe a
unique [a] = Just a
unique _   = Nothing

class ContainsNode a where
  theNode :: a -> Node

  nodeId :: a -> Nid
  nodeId = nid . theNode

  findeByNid :: [a] -> Nid -> Maybe a
  findeByNid as nid = unique $ filter (matchNid nid) as

  findeByNidError :: String -> [a] -> Nid -> a
  findeByNidError msg as nid = fromMaybe
    (error $ msg ++ " (" ++ show nid ++ ") nicht gefunden")
    (findeByNid as nid)

  matchNid :: Nid -> a -> Bool
  matchNid nid a = nid == nodeId a
  
  --titel :: a->String
  --titel = ntitel. theNode

data Thema = Thema
  { tnode :: Node
  , raum :: Maybe Raum
  , tbeamer :: Bool
  , mussStattfindenAn :: [ Zeiteinheit ]
  , voraussetzungen :: [ Thema ]
  }
  deriving (Show, Eq, Ord, Read)

instance LPVar Thema String where
  var (Thema (Node nid _) _ _ _ _) = "thema " ++ show nid

instance ContainsNode Thema where
  theNode = tnode

data ZeiteinheitTyp = Physikeinheit | Exkursion | Anderes
  deriving (Show, Eq, Ord, Read)

data Zeiteinheit = Zeiteinheit
  { znode :: Node
  , zTyp :: ZeiteinheitTyp
  , zeit :: String
  }
  deriving (Show, Eq, Ord, Read)

instance ContainsNode Zeiteinheit where
  theNode = znode

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
  deriving (Show, Eq, Ord, Read)

instance LPVar Raum String where
  var (Raum (Node nid _) _  _ _) = "raum " ++ show nid

instance ContainsNode Raum where
  theNode = rnode

data Themenwahl = Themenwahl
  { gewaehltesThema :: Thema
  , praeferenz      :: Double
  }
  deriving (Eq, Ord, Show, Read)

data Person = Person
  { uid      :: Uid
  , vorname  :: String
  , nachname :: String
  , verpasst :: [Zeiteinheit]
  }
  deriving (Eq, Ord, Show, Read)

data SchuelerIn = SchuelerIn
  { sPerson      :: Person
  , themenwahlen :: [ Themenwahl ]
  }
  deriving (Eq, Ord, Show, Read)

instance LPVar SchuelerIn String where
  var (SchuelerIn (Person uid _ _ _) _) = "schuelerin " ++ show uid

data BetreuerIn = BetreuerIn
  { bPerson        :: Person
  , betreuteThemen :: [ Themenwahl ]
  }
  deriving (Eq, Ord, Show, Read)

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
  deriving(Show, Read)

data GlobalBelegung = GlobalBelegung
  { gbThema       :: Thema
  , gbZeiteinheit :: Zeiteinheit
  }
  deriving (Eq, Ord, Show, Read)


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
  deriving (Eq, Ord, Show, Read)


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
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)

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
  deriving (Show, Read)

data LokalStundenplan = LokalStundenplan
  { globalStundenplan :: GlobalStundenplan
  , lokalBelegungen   :: [ LokalBelegung ]
  }
  deriving (Show, Read)
