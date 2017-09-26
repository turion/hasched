{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Stundenplan where

-- hasched
import LP.Utils

-- GenericPretty
import Text.PrettyPrint.GenericPretty

-- base
import Control.Arrow (first)
import Data.List
import Data.Maybe (fromMaybe)



newtype Nid = Nid Integer
  deriving (Ord, Eq, Num, Generic)

instance Out Nid where

-- Konstruktor muss bei read umgangen werden
instance Read Nid where
  readsPrec prec = map (first Nid) . readsPrec prec

instance Show Nid where
  show (Nid i) = show i

newtype Uid = Uid Integer
  deriving (Ord, Eq, Num, Generic)

instance Out Uid where

instance Read Uid where
  readsPrec prec = map (first Uid) . readsPrec prec

instance Show Uid where
  show (Uid i) = show i

data Node = Node
  { nid   :: Nid
  , titel :: String
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance Out Node where

unique :: [a] -> Maybe a
unique [a] = Just a
unique _   = Nothing

findBy :: Eq b => (a -> b) -> [a] -> a -> Maybe a
findBy f as a = unique $ filter ((f a ==) . f) as

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

  nTitel :: a -> String
  nTitel = titel . theNode

data Thema = Thema
  { tnode             :: Node
  , raum              :: Maybe Raum
  , tbeamer           :: Bool
  , mussStattfindenAn :: [ Zeiteinheit ]
  , voraussetzungen   :: [ Thema ]
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance Out Thema where

instance LPVar Thema String where
  var (Thema (Node nid _) _ _ _ _) = "thema " ++ show nid

instance ContainsNode Thema where
  theNode = tnode

data ZeiteinheitTyp = Physikeinheit | Exkursion | Anderes
  deriving (Show, Eq, Ord, Read, Generic)

instance Out ZeiteinheitTyp where

data Zeiteinheit = Zeiteinheit
  { znode :: Node
  , zTyp  :: ZeiteinheitTyp
  , zeit  :: String
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance Out Zeiteinheit where

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
  { rnode           :: Node
  , raumgroesse     :: Int
  , rbeamer         :: Bool
  , nichtVerfuegbar :: [ Zeiteinheit ]
  }
  deriving (Show, Eq, Ord, Read, Generic)

instance Out Raum where

instance LPVar Raum String where
  var (Raum (Node nid _) _  _ _) = "raum " ++ show nid

instance ContainsNode Raum where
  theNode = rnode

data Themenwahl = Themenwahl
  { gewaehltesThema :: Thema
  , praeferenz      :: Double
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance Out Themenwahl where

data Person = Person
  { uid      :: Uid
  , vorname  :: String
  , nachname :: String
  , verpasst :: [ Zeiteinheit ]
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance Out Person where

data SchuelerIn = SchuelerIn
  { sPerson      :: Person
  , themenwahlen :: [ Themenwahl ]
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance Out SchuelerIn where

instance LPVar SchuelerIn String where
  var (SchuelerIn (Person uid _ _ _) _) = "schuelerin " ++ show uid

data BetreuerIn = BetreuerIn
  { bPerson        :: Person
  , betreuteThemen :: [ Themenwahl ]
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance Out BetreuerIn where

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
  deriving (Show, Read, Generic)

instance Out Seminar where

data GlobalBelegung = GlobalBelegung
  { gbThema       :: Thema
  , gbZeiteinheit :: Zeiteinheit
  }
  deriving (Eq, Ord, Show, Read, Generic)

instance Out GlobalBelegung where

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
  deriving (Eq, Ord, Show, Read, Generic)

instance Out BetreuerBelegung where


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
  deriving (Eq, Ord, Show, Read, Generic)

instance Out RaumBelegung where

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
  deriving (Eq, Ord, Show, Read, Generic)

instance Out LokalBelegung where

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
  deriving (Show, Read, Generic)

instance Out GlobalStundenplan where

data LokalStundenplan = LokalStundenplan
  { globalStundenplan :: GlobalStundenplan
  , lokalBelegungen   :: [ LokalBelegung ]
  }
  deriving (Show, Read, Generic)

instance Out LokalStundenplan where


class Strip a where
  strip :: a -> a
  unstrip :: Seminar -> a -> Maybe a

instance Strip a => Strip [a] where
  strip = map strip
  unstrip seminar as = sequence $ unstrip seminar <$> as

instance Strip Thema where
  strip thema = thema
    { raum = Nothing
    , mussStattfindenAn = []
    , voraussetzungen   = []
    }
  unstrip Seminar {..} = findeByNid themen . nodeId

instance Strip Raum where
  strip raum = raum
    { nichtVerfuegbar = [] }
  unstrip Seminar {..} = findeByNid raeume . nodeId

instance Strip SchuelerIn where
  strip schuelerIn = schuelerIn
    { sPerson = (sPerson schuelerIn) { verpasst = [] }
    , themenwahlen = []
    }
  unstrip Seminar {..} = findBy (uid . sPerson) schuelerInnen

instance Strip BetreuerIn where
  strip betreuerIn = betreuerIn
    { bPerson = (bPerson betreuerIn) { verpasst = [] }
    , betreuteThemen = []
    }
  unstrip Seminar {..} = findBy (uid . bPerson) betreuerInnen


-- TODO Easier with Generic?
instance Strip Seminar where
  strip Seminar {..} = Seminar
    semnode
    (strip schuelerInnen)
    (strip betreuerInnen)
    (strip themen)
    zeiteinheiten
    (strip raeume)
  unstrip seminar _ = Just seminar

instance Strip GlobalBelegung where
  strip GlobalBelegung {..} = GlobalBelegung
    (strip gbThema)
    gbZeiteinheit
  unstrip seminar GlobalBelegung {..} = do
    gbThema       <- unstrip seminar gbThema
    return GlobalBelegung {..}

instance Strip BetreuerBelegung where
  strip BetreuerBelegung {..} = BetreuerBelegung
    (strip bGlobalBelegung)
    (strip bBetreuerIn)
  unstrip seminar BetreuerBelegung {..} = do
    bGlobalBelegung <- unstrip seminar bGlobalBelegung
    bBetreuerIn     <- unstrip seminar bBetreuerIn
    return BetreuerBelegung {..}

instance Strip RaumBelegung where
  strip RaumBelegung {..} = RaumBelegung
    (strip rGlobalBelegung)
    (strip rRaum)
  unstrip seminar RaumBelegung {..} = do
    rGlobalBelegung <- unstrip seminar rGlobalBelegung
    rRaum           <- unstrip seminar rRaum
    return RaumBelegung {..}

instance Strip LokalBelegung where
  strip LokalBelegung {..} = LokalBelegung
    (strip lGlobalBelegung)
    (strip lSchuelerIn)
  unstrip seminar LokalBelegung {..} = do
    lGlobalBelegung <- unstrip seminar lGlobalBelegung
    lSchuelerIn     <- unstrip seminar lSchuelerIn
    return LokalBelegung {..}

instance Strip GlobalStundenplan where
  strip GlobalStundenplan {..} = GlobalStundenplan
    (strip seminar)
    (strip globalBelegungen)
    (strip betreuerBelegungen)
    (strip raumBelegungen)
    version
  unstrip seminar' GlobalStundenplan {..} = do
    let seminar = seminar'
    globalBelegungen   <- unstrip seminar globalBelegungen
    betreuerBelegungen <- unstrip seminar betreuerBelegungen
    raumBelegungen     <- unstrip seminar raumBelegungen
    return GlobalStundenplan {..}

instance Strip LokalStundenplan where
  strip LokalStundenplan {..} = LokalStundenplan
    (strip globalStundenplan)
    (strip lokalBelegungen)
  unstrip seminar LokalStundenplan {..} = do
    globalStundenplan <- unstrip seminar globalStundenplan
    lokalBelegungen   <- unstrip seminar lokalBelegungen
    return LokalStundenplan {..}
