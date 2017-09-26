{-# LANGUAGE RecordWildCards #-}
module LP.Betreuer where

import LP.Imports


betreuerNotwendigkeiten :: LPSeminarFun
betreuerNotwendigkeiten seminar = do
  -- Ein Thema kann nur stattfinden, wenn BetreuerInnen dafür eingeteilt werden
  themaNurMitBetreuer seminar
  -- BetreuerInnen werden nur eingeteilt, wenn das Thema stattfindet
  betreuerNurFallsThemaStattfindet seminar
  -- BetreuerInnen können zu einer Zeit höchstens an einem Ort sein
  betreuerKoennenSichNichtSpalten seminar

themaNurMitBetreuer :: LPSeminarFun
themaNurMitBetreuer seminar =
  forM_ (moeglicheGlobalBelegungen seminar) $ \gb -> do
    varLF gb `leq` add
      [ varLF $ BetreuerBelegung gb betreuerIn
        | betreuerIn <- betreuerInnen seminar
      ]

betreuerNurFallsThemaStattfindet :: LPSeminarFun
betreuerNurFallsThemaStattfindet seminar  =
  forM_ (moeglicheBetreuerBelegungen seminar) $ \bb -> do
    varLF bb `leq` varLF (bGlobalBelegung bb)


betreuerKoennenSichNichtSpalten :: LPSeminarFun
betreuerKoennenSichNichtSpalten seminar =
  sequence_ $ do
    betreuerIn <- betreuerInnen seminar
    zeiteinheit <- zeiteinheiten seminar
    return $ add
      [ varLF $ BetreuerBelegung (GlobalBelegung thema zeiteinheit) betreuerIn
        | thema <- themen seminar
      ] `leqTo` 1

betreuerInnenVerpassen :: LPSeminarFun
betreuerInnenVerpassen Seminar {..} = sequence_ $ do
  betreuerIn  <- betreuerInnen
  zeiteinheit <- verpasst $ bPerson betreuerIn
  thema       <- themen
  return $ varLF (BetreuerBelegung (GlobalBelegung thema zeiteinheit) betreuerIn) `equalTo` 0


gewuenschtVonBetreuerIn :: BetreuerIn -> [Thema]
gewuenschtVonBetreuerIn BetreuerIn {..} = gewaehltesThema <$> filter ((100 ==) . praeferenz) betreuteThemen

-- TODO Testen
zugeordneteBetreuerInnen :: Seminar -> [(Thema, [BetreuerIn])]
zugeordneteBetreuerInnen Seminar {..} = [ (thema, filter (elem thema . gewuenschtVonBetreuerIn) betreuerInnen) | thema <- themen ]

-- | BetreuerInnen wünschen sich manchmal bestimmte Themen
betreuerInnenZuordnungen :: LPSeminarFun
betreuerInnenZuordnungen seminar = sequence_ $ do
  (thema, zugeordnete) <- zugeordneteBetreuerInnen seminar
  guard $ not $ null zugeordnete
  andereBetreuerIn <- betreuerInnen seminar
  guard $ not $ andereBetreuerIn `elem` zugeordnete
  zeiteinheit <- zeiteinheiten seminar
  return $ varLF (BetreuerBelegung (GlobalBelegung thema zeiteinheit) andereBetreuerIn) `equalTo` 0
