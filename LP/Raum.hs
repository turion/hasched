{-# LANGUAGE RecordWildCards #-}
module LP.Raum where

import LP.Imports

raumNotwendigkeiten :: LPSeminarFun
raumNotwendigkeiten seminar = do
  -- Ein Raum wird nur belegt, wenn dort etwas stattfindet
  raumNichtUnnoetigBelegen seminar
  -- Für jedes Thema muss ein Raum gebucht sein
  themaMussRaumHaben seminar
  -- TODO Raumgrößen, Raumausnahmen und weitere Ausnahmen
  -- In einem Raum kann zu einer Zeit höchstens ein Thema stattfinden
  raumNichtDoppeltBelegen seminar

raumNichtUnnoetigBelegen :: LPSeminarFun
raumNichtUnnoetigBelegen seminar =
 forM_ (moeglicheRaumBelegungen seminar) $ \rb -> do
    varLF rb `leq` varLF (rGlobalBelegung rb)

themaMussRaumHaben :: LPSeminarFun
themaMussRaumHaben seminar =
  sequence_ $ do
    thema <- themen seminar
    zeiteinheit <- zeiteinheiten seminar
    let gb = GlobalBelegung thema zeiteinheit
    return $ varLF gb `leq` add
      [ varLF $ RaumBelegung gb raum
        | raum <- raeume seminar
      ]

raumNichtDoppeltBelegen :: LPSeminarFun
raumNichtDoppeltBelegen seminar =
 sequence_ $ do
    raum <- raeume seminar
    zeiteinheit <- zeiteinheiten seminar
    return $ add
      [ varLF $ RaumBelegung (GlobalBelegung thema zeiteinheit) raum
        | thema <- themen seminar
      ] `leqTo` 1

raumAusnahmen :: LPSeminarFun
raumAusnahmen Seminar {..} = sequence_ $ do
  raum  <- raeume
  zeit  <- nichtVerfuegbar raum
  thema <- themen
  return $ varLF (RaumBelegung (GlobalBelegung thema zeit) raum) `equalTo` 0
