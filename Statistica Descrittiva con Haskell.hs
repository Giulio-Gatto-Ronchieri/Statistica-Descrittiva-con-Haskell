main :: IO ()
main = return ()

--------------------------------------------------------------------------------
--                                                                            --
--                                  PARTE I                                   --
--                                                                            --
--                     STATISTICA DESCRITTIVA UNIVARIATA                      --
--                                                                            --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- INSERIMENTO DELLA SCALA DI MISURA                                          --
--------------------------------------------------------------------------------

type NomeFile = String
type Modalità = String


-- Funzione che legge un file csv restituendo una lista di String, dove ogni String è una riga del file
insScala :: NomeFile -> IO [Modalità]
insScala nomeFile = do
   dati <- readFile nomeFile
   let scala = lines dati
--   print scala
   return scala

creaScalaNom :: [Modalità] -> ScalaNom
creaScalaNom sc = MakeScalaNom sc

-- Le modalità della scala ordinale devono essere statisticamente ordinate in modo tale da rispecchiare
-- l'ordinamento alfanumerico dei loro nomi all'interno del file
creaScalaOrd :: [Modalità] -> ScalaOrd
creaScalaOrd sc = MakeScalaOrd sc

creaScalaNum :: [Modalità] -> ScalaNum
creaScalaNum sc = MakeScalaNum (map read sc)

--------------------------------------------------------------------------------
-- RILEVAZIONE                                                                --
--------------------------------------------------------------------------------

type Unità_Statistica = String
type Osservazione = (Unità_Statistica, Modalità)
type Variabile = [Osservazione]


myWords :: String -> [String]
myWords s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : myWords s''
                            where (w, s'') = break (==',') s'

toTuple :: [String] -> (String, String)
toTuple (x:xs) = (x, head xs)
toTuple _      = undefined

-- Funzione che legge un file csv CON DUE COLONNE e restituisce una lista di coppie di String, dove:
--  - Ogni coppia di String è una riga del file e rappresenta un'osservazione;
--     - Il primo String di ogni coppia è il nome dell'unità statistica dell'osservazione;
--     - Il secondo String di ogni coppia è la modalità osservata nella particolare osservazione.
rilevazione :: NomeFile -> IO Variabile
rilevazione nomeFile = do
   dati <- readFile nomeFile
   let righe = lines dati
       parole = map myWords righe
       variabile = map toTuple parole
   return variabile

--------------------------------------------------------------------------------
-- SCALE DI MISURA                                                            --
--------------------------------------------------------------------------------

data ScalaNom = forall modalità. (Show modalità, Eq modalità) => MakeScalaNom [modalità]
data ScalaOrd = forall modalità. (Show modalità, Eq modalità, Ord modalità) => MakeScalaOrd [modalità]
data ScalaNum = forall modalità. (Show modalità, Eq modalità, Ord modalità, Real modalità) => MakeScalaNum [modalità]


data MyEither a b c = MyLeft a | MyMiddle b | MyRight c

count :: Eq a => a -> [a] -> Int
count _ []     = 0
count x (y:ys) = if x == y
                  then 1 + count x ys
                  else 0 + count x ys

class Scala a where
  conteggio :: a -> Variabile -> DistAssNon_Cum
  creaScalaBiNom :: a -> ScalaNom -> ScalaBiNom
  creaScalaBiOrd :: a -> ScalaOrd -> ScalaBiOrd
  creaScalaBiNum :: a -> ScalaNum -> ScalaBiNum
--
instance Scala ScalaNom where
  conteggio (MakeScalaNom sc) var = MyLeft (MakeDistAssNom [(c, count (show c) [b | (_,b) <- var]) | c <- sc])
  creaScalaBiNom a b = (MyLeft (a, b))
  creaScalaBiOrd a b = (MyLeft (a, b))
  creaScalaBiNum a b = (MyLeft (a, b))
--
instance Scala ScalaOrd where
  conteggio (MakeScalaOrd sc) var = MyMiddle (MakeDistAssOrd [(c, count (show c) [b | (_,b) <- var]) | c <- sc])
  creaScalaBiNom a b = (MyMiddle (b, a))
  creaScalaBiOrd a b = (MyMiddle (a, b))
  creaScalaBiNum a b = (MyMiddle (a, b))
--
instance Scala ScalaNum where
  conteggio (MakeScalaNum sc) var = MyRight (MakeDistAssNum [(c, count (show c) [b | (_,b) <- var]) | c <- sc])
  creaScalaBiNom a b = (MyRight (b, a))
  creaScalaBiOrd a b = (MyRight (b, a))
  creaScalaBiNum a b = (MyRight (a, b))

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE                                                 --
--------------------------------------------------------------------------------

data DistAssNom = forall modalità. (Show modalità, Eq modalità) => MakeDistAssNom [(modalità, Int)]
data DistAssOrd = forall modalità. (Show modalità, Eq modalità, Ord modalità) => MakeDistAssOrd [(modalità, Int)]
data DistAssNum = forall modalità. (Show modalità, Eq modalità, Ord modalità, Real modalità) => MakeDistAssNum [(modalità, Int)]
--
data DistRelNom = forall modalità. (Show modalità, Eq modalità) => MakeDistRelNom [(modalità, Float)]
data DistRelOrd = forall modalità. (Show modalità, Eq modalità, Ord modalità) => MakeDistRelOrd [(modalità, Float)]
data DistRelNum = forall modalità. (Show modalità, Eq modalità, Ord modalità, Real modalità) => MakeDistRelNum [(modalità, Float)]
--
data DistAssCumOrd = forall modalità. (Show modalità, Eq modalità, Ord modalità) => MakeDistAssCumOrd [(modalità, Int)]
data DistAssCumNum = forall modalità. (Show modalità, Eq modalità, Ord modalità, Real modalità) => MakeDistAssCumNum [(modalità, Int)]
--
data DistRelCumOrd = forall modalità. (Show modalità, Eq modalità, Ord modalità) => MakeDistRelCumOrd [(modalità, Float)]
data DistRelCumNum = forall modalità. (Show modalità, Eq modalità, Ord modalità, Real modalità) => MakeDistRelCumNum [(modalità, Float)]


sommaDist :: [(a, Int)] -> Int
sommaDist dist = sum [a | (_,a) <- dist]

ultimoDist :: [(a, Int)] -> Int
ultimoDist dist = last [a | (_,a) <- dist]

class Dist a where
  totale :: a -> Int
  gradi :: a -> Int

  posizioneNon_AnStrict :: a -> PosizioneNon_An
  posizioneNon_An :: a -> PosizioneNon_An
  posizioneAnStrict :: a -> PosizioneAn
  posizioneAn :: a -> PosizioneAn
  posizioneStrict :: a -> Posizione
  posizione :: a -> Posizione

  eterogeneitàStrict :: a -> Eterogeneità
  eterogeneità :: a -> Eterogeneità
  disuguaglianzaStrict :: a -> Disuguaglianza
  disuguaglianza :: a -> Disuguaglianza
  dispersioneStrict :: a -> Dispersione
  dispersione :: a -> Dispersione
  variabilitàStrict :: a -> Variabilità
  variabilità :: a -> Variabilità

  simmetriaStrict :: a -> Simmetria
  simmetria :: a -> Simmetria
  appiattimentoStrict :: a -> Appiattimento
  appiattimento :: a -> Appiattimento
  formaStrict :: a -> Forma
  forma :: a -> Forma

  summaryStrict :: a -> Indici
  summary :: a -> Indici
--
instance Dist DistAssNom where
  totale (MakeDistAssNom dist) = sommaDist dist
  gradi (MakeDistAssNom dist) = length dist

  posizioneNon_AnStrict dist = (Just (moda dist), Nothing)
  posizioneNon_An dist = posizioneNon_AnStrict dist
  posizioneAnStrict dist = Nothing
  posizioneAn dist = posizioneAnStrict dist
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Nothing
  eterogeneità dist =
     let Left (MyLeft a) = relativizzazione dist
     in  Just (giniNorm a, entropiaNorm a)
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Nothing)
  disuguaglianza dist = (eterogeneità dist, Nothing)
  dispersioneStrict dist = Nothing
  dispersione dist = dispersioneStrict dist
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Nothing
  simmetria dist = simmetriaStrict dist
  appiattimentoStrict dist = Nothing
  appiattimento dist = appiattimentoStrict dist
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)
--
instance Dist DistAssOrd where
  totale (MakeDistAssOrd dist) = sommaDist dist
  gradi (MakeDistAssOrd dist) = length dist

  posizioneNon_AnStrict dist = (Just (moda dist), Nothing)
  posizioneNon_An dist =
     let Left (Left a) = cumulazione dist
         Right (Left b) = relativizzazione a
     in  (Just (moda dist), Just (mediana b))
  posizioneAnStrict dist = Nothing
  posizioneAn dist = posizioneAnStrict dist
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Nothing
  eterogeneità dist =
     let Left (MyMiddle a) = relativizzazione dist
     in  Just (giniNorm a, entropiaNorm a)
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Nothing)
  disuguaglianza dist = (eterogeneità dist, Nothing)
  dispersioneStrict dist = Nothing
  dispersione dist = dispersioneStrict dist
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Nothing
  simmetria dist = simmetriaStrict dist
  appiattimentoStrict dist = Nothing
  appiattimento dist = appiattimentoStrict dist
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)
--
instance Dist DistAssNum where
  totale (MakeDistAssNum dist) = sommaDist dist
  gradi (MakeDistAssNum dist) = length dist

  posizioneNon_AnStrict dist = (Just (moda dist), Nothing)
  posizioneNon_An dist =
     let Left (Right a) = cumulazione dist
         Right (Right b) = relativizzazione a
     in  (Just (moda dist), Just (mediana b))
  posizioneAnStrict dist = Just (minimo dist, mediaArm dist, mediaGeom dist, mediaArm dist, massimo dist)
  posizioneAn dist = posizioneAnStrict dist
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Nothing
  eterogeneità dist =
     let Left (MyRight a) = relativizzazione dist
      in  Just (giniNorm a, entropiaNorm a)
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Just (range dist))
  disuguaglianza dist = (eterogeneità dist, Just (range dist))
  dispersioneStrict dist = Just (varianza dist, sqm dist, coeffVar dist)
  dispersione dist = dispersioneStrict dist
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Just (asimmFisher dist)
  simmetria dist = simmetriaStrict dist
  appiattimentoStrict dist = Just (curtosiPearson dist, curtosiFisher dist)
  appiattimento dist = appiattimentoStrict dist
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)
--
--
instance Dist DistRelNom where
  totale (MakeDistRelNom dist) = 1 :: Int
  gradi (MakeDistRelNom dist) = length dist

  posizioneNon_AnStrict dist = (Just (moda dist), Nothing)
  posizioneNon_An dist = posizioneNon_AnStrict dist
  posizioneAnStrict dist = Nothing
  posizioneAn dist = posizioneAnStrict dist
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Just (giniNorm dist, entropiaNorm dist)
  eterogeneità dist = eterogeneitàStrict dist
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Nothing)
  disuguaglianza dist = (eterogeneità dist, Nothing)
  dispersioneStrict dist = Nothing
  dispersione dist = dispersioneStrict dist
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Nothing
  simmetria dist = simmetriaStrict dist
  appiattimentoStrict dist = Nothing
  appiattimento dist = appiattimentoStrict dist
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)
--
instance Dist DistRelOrd where
  totale (MakeDistRelOrd dist) = 1 :: Int
  gradi (MakeDistRelOrd dist) = length dist

  posizioneNon_AnStrict dist = (Just (moda dist), Nothing)
  posizioneNon_An dist =
     let Right (Left a) = cumulazione dist
     in  (Just (moda dist), Just (mediana a))
  posizioneAnStrict dist = Nothing
  posizioneAn dist = posizioneAnStrict dist
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Just (giniNorm dist, entropiaNorm dist)
  eterogeneità dist = eterogeneitàStrict dist
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Nothing)
  disuguaglianza dist = (eterogeneità dist, Nothing)
  dispersioneStrict dist = Nothing
  dispersione dist = dispersioneStrict dist
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Nothing
  simmetria dist = simmetriaStrict dist
  appiattimentoStrict dist = Nothing
  appiattimento dist = appiattimentoStrict dist
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)
--
instance Dist DistRelNum where
  totale (MakeDistRelNum dist) = 1 :: Int
  gradi (MakeDistRelNum dist) = length dist

  posizioneNon_AnStrict dist = (Just (moda dist), Nothing)
  posizioneNon_An dist =
     let Right (Right a) = cumulazione dist
     in  (Just (moda dist), Just (mediana a))
  posizioneAnStrict dist = Just (minimo dist, mediaArm dist, mediaGeom dist, mediaArm dist, massimo dist)
  posizioneAn dist = posizioneAnStrict dist
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Just (giniNorm dist, entropiaNorm dist)
  eterogeneità dist = eterogeneitàStrict dist
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Just (range dist))
  disuguaglianza dist = (eterogeneità dist, Just (range dist))
  dispersioneStrict dist = Just (varianza dist, sqm dist, coeffVar dist)
  dispersione dist = dispersioneStrict dist
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Just (asimmFisher dist)
  simmetria dist = simmetriaStrict dist
  appiattimentoStrict dist = Just (curtosiPearson dist, curtosiFisher dist)
  appiattimento dist = appiattimentoStrict dist
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)
--
--
instance Dist DistAssCumOrd where
  totale (MakeDistAssCumOrd dist) = ultimoDist dist
  gradi (MakeDistAssCumOrd dist) = length dist

  posizioneNon_AnStrict dist = (Nothing, Nothing)
  posizioneNon_An dist =
     let Left (Left a) = decumulazione dist
         Right (Left b) = relativizzazione dist
     in  (Just (moda a), Just (mediana b))
  posizioneAnStrict dist = Nothing
  posizioneAn dist = posizioneAnStrict dist
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Nothing
  eterogeneità dist =
     let Left (Left a) = decumulazione dist
         Left (MyMiddle b) = relativizzazione a
     in  Just (giniNorm b, entropiaNorm b)
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Nothing)
  disuguaglianza dist = (eterogeneità dist, Nothing)
  dispersioneStrict dist = Nothing
  dispersione dist = dispersioneStrict dist
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Nothing
  simmetria dist = simmetriaStrict dist
  appiattimentoStrict dist = Nothing
  appiattimento dist = appiattimentoStrict dist
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)
--
instance Dist DistAssCumNum where
  totale (MakeDistAssCumNum dist) = ultimoDist dist
  gradi (MakeDistAssCumNum dist) = length dist

  posizioneNon_AnStrict dist = (Nothing, Nothing)
  posizioneNon_An dist =
     let Left (Right a) = decumulazione dist
         Right (Right b) = relativizzazione dist
     in  (Just (moda a), Just (mediana b))
  posizioneAnStrict dist = Nothing
  posizioneAn dist =
     let Left (Right a) = decumulazione dist
     in  Just (minimo a, mediaArm a, mediaGeom a, mediaArm a, massimo a)
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Nothing
  eterogeneità dist =
     let Left (Right a) = decumulazione dist
         Left (MyRight b) = relativizzazione a
     in  Just (giniNorm b, entropiaNorm b)
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Nothing)
  disuguaglianza dist =
     let Left (Right a) = decumulazione dist
     in  (eterogeneità dist, Just (range a))
  dispersioneStrict dist = Nothing
  dispersione dist =
     let Left (Right a) = decumulazione dist
     in  Just (varianza a, sqm a, coeffVar a)
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Nothing
  simmetria dist =
     let Left (Right a) = decumulazione dist
     in  Just (asimmFisher a)
  appiattimentoStrict dist = Nothing
  appiattimento dist =
     let Left (Right a) = decumulazione dist
     in  Just (curtosiPearson a, curtosiFisher a)
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)
--
--
instance Dist DistRelCumOrd where
  totale (MakeDistRelCumOrd dist) = 1 :: Int
  gradi (MakeDistRelCumOrd dist) = length dist

  posizioneNon_AnStrict dist = (Nothing, Just (mediana dist))
  posizioneNon_An dist =
     let Right (Left a) = decumulazione dist
     in  (Just (moda a), Just (mediana dist))
  posizioneAnStrict dist = Nothing
  posizioneAn dist = posizioneAnStrict dist
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Nothing
  eterogeneità dist =
     let Right (Left a) = decumulazione dist
     in  Just (giniNorm a, entropiaNorm a)
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Nothing)
  disuguaglianza dist = (eterogeneità dist, Nothing)
  dispersioneStrict dist = Nothing
  dispersione dist = dispersioneStrict dist
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Nothing
  simmetria dist = simmetriaStrict dist
  appiattimentoStrict dist = Nothing
  appiattimento dist = appiattimentoStrict dist
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)
--
instance Dist DistRelCumNum where
  totale (MakeDistRelCumNum dist) = 1 :: Int
  gradi (MakeDistRelCumNum dist) = length dist

  posizioneNon_AnStrict dist = (Nothing, Just (mediana dist))
  posizioneNon_An dist =
     let Right (Right a) = decumulazione dist
     in  (Just (moda a), Just (mediana dist))
  posizioneAnStrict dist = Nothing
  posizioneAn dist =
     let Right (Right a) = decumulazione dist
     in  Just (minimo a, mediaArm a, mediaGeom a, mediaArm a, massimo a)
  posizioneStrict dist = (posizioneNon_AnStrict dist, posizioneAnStrict dist)
  posizione dist = (posizioneNon_An dist, posizioneAn dist)

  eterogeneitàStrict dist = Nothing
  eterogeneità dist =
     let Right (Right a) = decumulazione dist
     in  Just (giniNorm a, entropiaNorm a)
  disuguaglianzaStrict dist = (eterogeneitàStrict dist, Nothing)
  disuguaglianza dist =
     let Right (Right a) = decumulazione dist
     in  (eterogeneità dist, Just (range a))
  dispersioneStrict dist = Nothing
  dispersione dist =
     let Right (Right a) = decumulazione dist
     in  Just (varianza a, sqm a, coeffVar a)
  variabilitàStrict dist = (eterogeneitàStrict dist, dispersioneStrict dist)
  variabilità dist = (eterogeneità dist, dispersione dist)

  simmetriaStrict dist = Nothing
  simmetria dist =
     let Right (Right a) = decumulazione dist
     in  Just (asimmFisher a)
  appiattimentoStrict dist = Nothing
  appiattimento dist =
     let Right (Right a) = decumulazione dist
     in  Just (curtosiPearson a, curtosiFisher a)
  formaStrict dist = (simmetriaStrict dist, appiattimentoStrict dist)
  forma dist = (simmetria dist, appiattimento dist)

  summaryStrict dist = (posizioneStrict dist, variabilitàStrict dist, formaStrict dist)
  summary dist = (posizione dist, variabilità dist, forma dist)

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE ASSOLUTE                                        --
--------------------------------------------------------------------------------

type DistAss' = MyEither DistAssNom DistAssOrd DistAssNum


class DistAss a where
  relativizzazione :: a -> DistRel
--
instance DistAss DistAssNom where
  relativizzazione (MakeDistAssNom dist) = Left (MyLeft (MakeDistRelNom [(a, (fromIntegral b)/(fromIntegral (totale (MakeDistAssNom dist)))) | (a,b) <- dist]))
--
instance DistAss DistAssOrd where
  relativizzazione (MakeDistAssOrd dist) = Left (MyMiddle (MakeDistRelOrd [(a, (fromIntegral b)/(fromIntegral (totale (MakeDistAssOrd dist)))) | (a,b) <- dist]))
--
instance DistAss DistAssNum where
  relativizzazione (MakeDistAssNum dist) = Left (MyRight (MakeDistRelNum [(a, (fromIntegral b)/(fromIntegral (totale (MakeDistAssNum dist)))) | (a,b) <- dist]))
--
--
instance DistAss DistAssCumOrd where
  relativizzazione (MakeDistAssCumOrd dist) = Right (Left (MakeDistRelCumOrd [(a, (fromIntegral b)/(fromIntegral (totale (MakeDistAssCumOrd dist)))) | (a,b) <- dist]))
--
instance DistAss DistAssCumNum where
  relativizzazione (MakeDistAssCumNum dist) = Right (Right (MakeDistRelCumNum [(a, (fromIntegral b)/(fromIntegral (totale (MakeDistAssCumNum dist)))) | (a,b) <- dist]))

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE RELATIVE                                        --
--------------------------------------------------------------------------------

type DistRel = Either DistRelNon_Cum' DistRelCum'

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE ASSOLUTE NON CUMULATE                           --
--------------------------------------------------------------------------------

type DistAssNon_Cum = MyEither DistAssNom DistAssOrd DistAssNum

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE ORDINABILI                                      --
--------------------------------------------------------------------------------

type DistOrdin' = Either DistAssOrdin DistRelOrdin


cumul :: (Ord a, Real b) => [(a, b)] -> [(a, b)] -- si può anche mettere Num al posto di Real
cumul dist = [(a, sum [c | (b,c) <- dist, b <= a]) | (a,_) <- dist]

class DistOrdin a where
  cumulazione :: a -> DistCum'
--
instance DistOrdin DistAssOrd where
  cumulazione (MakeDistAssOrd dist) = Left (Left (MakeDistAssCumOrd (cumul dist)))
--
instance DistOrdin DistAssNum where
  cumulazione (MakeDistAssNum dist) = Left (Right (MakeDistAssCumNum (cumul dist)))
--
instance DistOrdin DistRelOrd where
  cumulazione (MakeDistRelOrd dist) = Right (Left (MakeDistRelCumOrd (cumul dist)))
--
instance DistOrdin DistRelNum where
  cumulazione (MakeDistRelNum dist) = Right (Right (MakeDistRelCumNum (cumul dist)))

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE ASSOLUTE ORDINABILI                             --
--------------------------------------------------------------------------------

type DistAssOrdin = Either DistAssOrd DistAssNum

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE RELATIVE ORDINABILI                             --
--------------------------------------------------------------------------------

type DistRelOrdin = Either DistRelOrd DistRelNum

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE CUMULATE                                        --
--------------------------------------------------------------------------------

type DistCum' = Either DistAssCum DistRelCum'
type DistAssCum = Either DistAssCumOrd DistAssCumNum

cumulInv :: (Ord a, Real b) => [(a, b)] -> [(a, b)] -- si può anche mettere Num al posto di Real
cumulInv dist = [(a, b - sum [d | (c,d) <- dist, c < a]) | (a,b) <- dist]

class DistCum a where
  decumulazione :: a -> DistOrdin'
--
instance DistCum DistAssCumOrd where
  decumulazione (MakeDistAssCumOrd dist) = Left (Left (MakeDistAssOrd (cumulInv dist)))
--
instance DistCum DistAssCumNum where
  decumulazione (MakeDistAssCumNum dist) = Left (Right (MakeDistAssNum (cumulInv dist)))
--
instance DistCum DistRelCumOrd where
  decumulazione (MakeDistRelCumOrd dist) = Right (Left (MakeDistRelOrd (cumulInv dist)))
--
instance DistCum DistRelCumNum where
  decumulazione (MakeDistRelCumNum dist) = Right (Right (MakeDistRelNum (cumulInv dist)))

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE RELATIVE CUMULATE                               --
--------------------------------------------------------------------------------

type DistRelCum' = Either DistRelCumOrd DistRelCumNum


data Med1 = forall modalità. (Show modalità, Eq modalità, Ord modalità) => MakeMed1 modalità
data Med2 = forall modalità. (Show modalità, Eq modalità, Ord modalità, Real modalità) => MakeMed2 modalità
type Mediana1 = Maybe Med1
type Mediana2 = Either Med2 (Med2, Med2)
type Mediana = Either Mediana1 Mediana2

class DistRelCum a where
  mediana :: a -> Mediana
--
instance DistRelCum DistRelCumOrd where
  mediana (MakeDistRelCumOrd dist) =
     let me1 = head [a | (a,b) <- dist, b >= 0.5]
         me2 = head [a | (a,b) <- dist, b > 0.5]
     in  if (me1 == me2)
            then
               Left (Just (MakeMed1 me1))
            else
               Left (Nothing)
--
instance DistRelCum DistRelCumNum where
  mediana (MakeDistRelCumNum dist) =
     let me1 = head [a | (a,b) <- dist, b >= 0.5]
         me2 = head [a | (a,b) <- dist, b > 0.5]
     in  if (me1 == me2)
            then
               Right (Left (MakeMed2 me1))
            else
               Right (Right (MakeMed2 me1, MakeMed2 me2))

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE NON CUMULATE                                    --
--------------------------------------------------------------------------------

data Moda = forall modalità. (Show modalità, Eq modalità) => MakeModa [modalità]


maxList :: Ord a => [a] -> a
maxList [x]    = x
maxList (x:xs) = max x (maxList xs)

mo :: (Show modalità, Eq modalità, Ord a) => [(modalità, a)] -> [modalità]
mo dist = [a | (a,b) <- dist, b == maxList [c | (_,c) <- dist]]

class DistNon_Cum a where
  moda :: a -> Moda
--
instance DistNon_Cum DistAssNom where
  moda (MakeDistAssNom dist) = MakeModa (mo dist)
--
instance DistNon_Cum DistAssOrd where
  moda (MakeDistAssOrd dist) = MakeModa (mo dist)
--
instance DistNon_Cum DistAssNum where
  moda (MakeDistAssNum dist) = MakeModa (mo dist)
--
--
instance DistNon_Cum DistRelNom where
  moda (MakeDistRelNom dist) = MakeModa (mo dist)
--
instance DistNon_Cum DistRelOrd where
  moda (MakeDistRelOrd dist) = MakeModa (mo dist)
--
instance DistNon_Cum DistRelNum where
  moda (MakeDistRelNum dist) = MakeModa (mo dist)

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE NON CUMULATE NUMERICHE                          --
--------------------------------------------------------------------------------

type MediaPot = Float
type Minimo = MediaPot
type MediaArm = MediaPot
type MediaGeom = MediaPot
type MediaArit = MediaPot
type Massimo = MediaPot
type Range = Float -- CAMPO DI VARIAZIONE, a.k.a. RANGE
--
type Momento = Float
type Devianza = Float -- utile per mostrare metodi alternativi di calcolo
type Varianza = Momento
type Sqm = Float -- SCARTO QUADRATICO MEDIO
type AsimmFisher = Float
type CurtosiPearson = Float
type CurtosiFisher = Float
type CV = Float
type CoeffVar = Maybe CV


minList :: Ord a => [a] -> a
minList [x]    = x
minList (x:xs) = min x (minList xs)


data Infinito = PiùInf | MenoInf

instance Eq Infinito where
  MenoInf == MenoInf = True
  PiùInf == PiùInf = True
  MenoInf == PiùInf = False
  PiùInf == MenoInf = False

type Ordine = Either Infinito Float

class DistNon_CumNum a where
  mediaPot :: Ordine -> a -> MediaPot
  minimo :: a -> Minimo
  mediaArm :: a -> MediaArm
  mediaGeom :: a -> MediaGeom
  mediaArit :: a -> MediaArit
  massimo :: a -> Massimo
  range :: a -> Range

  momento :: Int -> Either Float (a -> Float) -> a -> Momento
  mediaArit' :: a -> MediaArit -- definizione alternativa della media aritmetica come momento primo non centrato
  devianza :: a -> Devianza
  varianza :: a -> Varianza
  varianza' :: a -> Varianza -- definizione alternativa della varianza come devianza/n
  sqm :: a -> Sqm
  asimmFisher :: a -> AsimmFisher
  curtosiPearson :: a -> CurtosiPearson
  curtosiFisher :: a -> CurtosiFisher
  isConcorde :: a -> Bool
  coeffVar :: a -> CoeffVar
--
--
instance DistNon_CumNum DistAssNum where
  mediaPot (Left k) (MakeDistAssNum dist)  | k == MenoInf = realToFrac (minList [a | (a,b) <- dist, b > 0])
                                           | k == PiùInf  = realToFrac (maxList [a | (a,b) <- dist, b > 0])
  mediaPot (Right k) (MakeDistAssNum dist) | k == 0       = (product [(realToFrac a)^^b | (a,b) <- dist])**(1/fromIntegral (totale (MakeDistAssNum dist)))
                                           | otherwise    = ((sum [((realToFrac a)**k)*(fromIntegral b) | (a,b) <- dist])/(fromIntegral (totale (MakeDistAssNum dist))))**(1/k)
  minimo = mediaPot (Left MenoInf)
  mediaArm = mediaPot (Right (-1))
  mediaGeom = mediaPot (Right 0)
  mediaArit = mediaPot (Right 1)
  massimo = mediaPot (Left PiùInf)
  range dist = (massimo dist) - (minimo dist)

  momento k (Left c) (MakeDistAssNum dist) = (sum [(((realToFrac a)-c)^^k)*(fromIntegral b) | (a,b) <- dist])/(fromIntegral (totale (MakeDistAssNum dist)))
  momento k (Right func) (MakeDistAssNum dist) = (sum [(((realToFrac a) - func (MakeDistAssNum dist))^^k)*(fromIntegral b) | (a,b) <- dist])/(fromIntegral (totale (MakeDistAssNum dist)))
  mediaArit' = momento 1 (Left 0)
  devianza (MakeDistAssNum dist) = sum [(((realToFrac a) - (mediaArit (MakeDistAssNum dist)))^2)*(fromIntegral b) | (a,b) <- dist]
  varianza = momento 2 (Right mediaArit)
  varianza' dist = (devianza dist)/(fromIntegral (totale dist))
  sqm dist = sqrt (varianza dist)
  asimmFisher dist = (momento 3 (Right mediaArit) dist)/((sqm dist)^3)
  curtosiPearson dist = (momento 4 (Right mediaArit) dist)/((varianza dist)^2)
  curtosiFisher dist = (curtosiPearson dist) - 3
  isConcorde (MakeDistAssNum dist) | minList [a | (a,_) <- dist] >= 0 = True
                                   | maxList [a | (a,_) <- dist] <= 0 = True
                                   | otherwise                        = False
  coeffVar dist | ((mediaArit dist) /= 0) && (isConcorde dist) = Just ((sqm dist)/(abs (mediaArit dist)))
                | otherwise                                    = Nothing
--
instance DistNon_CumNum DistRelNum where
  mediaPot (Left k) (MakeDistRelNum dist)  | k == MenoInf = realToFrac (minList [a | (a,b) <- dist, b > 0])
                                           | k == PiùInf  = realToFrac (maxList [a | (a,b) <- dist, b > 0])
  mediaPot (Right k) (MakeDistRelNum dist) | k == 0       = product [(realToFrac a)**b | (a,b) <- dist]
                                           | otherwise    = (sum [((realToFrac a)**k)*b | (a,b) <- dist])**(1/k)
  minimo = mediaPot (Left MenoInf)
  mediaArm = mediaPot (Right (-1))
  mediaGeom = mediaPot (Right 0)
  mediaArit = mediaPot (Right 1)
  massimo = mediaPot (Left PiùInf)
  range dist = (massimo dist) - (minimo dist)

  momento k (Left c) (MakeDistRelNum dist) = sum [(((realToFrac a)-c)^^k)*b | (a,b) <- dist]
  momento k (Right func) (MakeDistRelNum dist) = sum [(((realToFrac a) - func (MakeDistRelNum dist))^^k)*b | (a,b) <- dist]
  mediaArit' = momento 1 (Left 0)
  devianza (MakeDistRelNum dist) = sum [(((realToFrac a) - (mediaArit (MakeDistRelNum dist)))^2)*b | (a,b) <- dist] -- SI PUò CALCOLARE PER DIST REL? SE Sì, ALLORA COINCIDE CON LA VARIANZA
  varianza = momento 2 (Right mediaArit)
  varianza' dist = (devianza dist) -- aggiungere /(fromIntegral (totale dist)) sarebbe equivalente visto che il totale è pari a 1
  sqm dist = sqrt (varianza dist)
  asimmFisher dist = (momento 3 (Right mediaArit) dist)/((sqm dist)^3)
  curtosiPearson dist = (momento 4 (Right mediaArit) dist)/((varianza dist)^2)
  curtosiFisher dist = (curtosiPearson dist) - 3
  isConcorde (MakeDistRelNum dist) | minList [a | (a,_) <- dist] >= 0 = True
                                   | maxList [a | (a,_) <- dist] <= 0 = True
                                   | otherwise                        = False
  coeffVar dist | ((mediaArit dist) /= 0) && (isConcorde dist) = Just ((sqm dist)/(abs (mediaArit dist)))
                | otherwise                                    = Nothing

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE RELATIVE NON CUMULATE                           --
--------------------------------------------------------------------------------

type DistRelNon_Cum' = MyEither DistRelNom DistRelOrd DistRelNum


type Gini = Float
type GiniMax = Float
type GiniNorm = Float

type Entropia = Float
type EntropiaMax = Float
type EntropiaNorm = Float


calcGini :: [(a, Float)] -> Gini
calcGini dist = 1 - sum [a^2 | (_,a) <- dist]

calcEntropia :: [(a, Float)] -> Entropia
calcEntropia dist = - sum [a*log(a) | (_,a) <- dist]

class DistRelNon_Cum a where
  gini :: a -> Gini
  giniMax :: a -> GiniMax
  giniNorm :: a -> GiniNorm

  entropia :: a -> Entropia
  entropiaMax :: a -> EntropiaMax
  entropiaNorm :: a -> EntropiaNorm
--
instance DistRelNon_Cum DistRelNom where
  gini (MakeDistRelNom dist) = calcGini dist
  giniMax dist = (fromIntegral (gradi dist) - 1)/(fromIntegral (gradi dist))
  giniNorm dist = (gini dist)/(giniMax dist)

  entropia (MakeDistRelNom dist) = calcEntropia dist
  entropiaMax dist = log(fromIntegral (gradi dist))
  entropiaNorm dist = (entropia dist)/(entropiaMax dist)
--
instance DistRelNon_Cum DistRelOrd where
  gini (MakeDistRelOrd dist) = calcGini dist
  giniMax dist = (fromIntegral (gradi dist) - 1)/(fromIntegral (gradi dist))
  giniNorm dist = (gini dist)/(giniMax dist)

  entropia (MakeDistRelOrd dist) = calcEntropia dist
  entropiaMax dist = log(fromIntegral (gradi dist))
  entropiaNorm dist = (entropia dist)/(entropiaMax dist)
--
instance DistRelNon_Cum DistRelNum where
  gini (MakeDistRelNum dist) = calcGini dist
  giniMax dist = (fromIntegral (gradi dist) - 1)/(fromIntegral (gradi dist))
  giniNorm dist = (gini dist)/(giniMax dist)

  entropia (MakeDistRelNum dist) = calcEntropia dist
  entropiaMax dist = log(fromIntegral (gradi dist))
  entropiaNorm dist = (entropia dist)/(entropiaMax dist)

--------------------------------------------------------------------------------
-- INDICI                                                                     --
--------------------------------------------------------------------------------

type Indici = (Posizione, Variabilità, Forma)

--------------------------------------------------------------------------------
-- INDICI DI POSIZIONE                                                        --
--------------------------------------------------------------------------------

type Posizione = (PosizioneNon_An, PosizioneAn)

--------------------------------------------------------------------------------
-- INDICI DI POSIZIONE NON ANALITICI                                          --
--------------------------------------------------------------------------------

type PosizioneNon_An = (Maybe Moda, Maybe Mediana)

--------------------------------------------------------------------------------
-- INDICI DI POSIZIONE ANALITICI                                              --
--------------------------------------------------------------------------------

type PosizioneAn = Maybe (Minimo, MediaArm, MediaGeom, MediaArit, Massimo)

--------------------------------------------------------------------------------
-- INDICI DI VARIABILITÀ                                                      --
--------------------------------------------------------------------------------

type Variabilità = (Eterogeneità, Dispersione)

--------------------------------------------------------------------------------
-- INDICI DI DISUGUAGLIANZA                                                   --
--------------------------------------------------------------------------------

type Disuguaglianza = (Eterogeneità, Maybe Range)

--------------------------------------------------------------------------------
-- INDICI DI ETEROGENEITÀ                                                     --
--------------------------------------------------------------------------------

type Eterogeneità = Maybe (GiniNorm, EntropiaNorm)

--------------------------------------------------------------------------------
-- INDICI DI DISPERSIONE                                                      --
--------------------------------------------------------------------------------

type Dispersione = Maybe (Varianza, Sqm, CoeffVar)

--------------------------------------------------------------------------------
-- INDICI DI FORMA                                                            --
--------------------------------------------------------------------------------

type Forma = (Simmetria, Appiattimento)

--------------------------------------------------------------------------------
-- INDICI DI SIMMETRIA                                                        --
--------------------------------------------------------------------------------

type Simmetria = Maybe (AsimmFisher)

--------------------------------------------------------------------------------
-- INDICI DI APPIATTIMENTO                                                    --
--------------------------------------------------------------------------------

type Appiattimento = Maybe (CurtosiPearson, CurtosiFisher)

--------------------------------------------------------------------------------
--                                                                            --
--                                  PARTE II                                  --
--                                                                            --
--                      STATISTICA DESCRITTIVA BIVARIATA                      --
--                                                                            --
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- RILEVAZIONE BIVARIATA                                                      --
--------------------------------------------------------------------------------

type OsservazioneBi = (Unità_Statistica, Modalità, Modalità)
type VariabileBi = [OsservazioneBi]

toTuple3 :: [String] -> (String, String, String)
toTuple3 (x:xs) = (x, head xs, head (tail xs))
toTuple3 _      = undefined

-- Funzione che legge un file csv CON TRE COLONNE e restituisce una lista di triplette di String, dove:
--  - Ogni tripletta di String è una riga del file e rappresenta un'osservazione;
--     - Il primo String di ogni tripletta è il nome dell'unità statistica dell'osservazione;
--     - Il secondo e il terzo String di ogni tripletta le modalità osservate nella particolare osservazione.
rilevazioneBi :: NomeFile -> IO VariabileBi
rilevazioneBi nomeFile = do
   dati <- readFile nomeFile
   let righe = lines dati
       parole = map myWords righe
       variabile = map toTuple3 parole
   return variabile

--------------------------------------------------------------------------------
-- "SCALE DI MISURA BIVARIATE"                                                --
--------------------------------------------------------------------------------

type ScalaNomNom = (ScalaNom, ScalaNom)
type ScalaNomOrd = (ScalaNom, ScalaOrd)
type ScalaNomNum = (ScalaNom, ScalaNum)
type ScalaOrdOrd = (ScalaOrd, ScalaOrd)
type ScalaOrdNum = (ScalaOrd, ScalaNum)
type ScalaNumNum = (ScalaNum, ScalaNum)

type ScalaBiNom = MyEither ScalaNomNom ScalaNomOrd ScalaNomNum
type ScalaBiOrd = MyEither ScalaNomOrd ScalaOrdOrd ScalaOrdNum
type ScalaBiNum = MyEither ScalaNomNum ScalaOrdNum ScalaNumNum


class ScalaBi a where
  conteggioBi :: a -> VariabileBi -> DistBi'
--
instance ScalaBi ScalaNomNom where
  conteggioBi (MakeScalaNom sc1, MakeScalaNom sc2) var = Left (MyLeft (MakeDistNomNom [((a, b), count (show a, show b) [(c,d) | (_,c,d) <- var]) | a <- sc1, b <- sc2]))
--
instance ScalaBi ScalaNomOrd where
  conteggioBi (MakeScalaNom sc1, MakeScalaOrd sc2) var = Left (MyMiddle (MakeDistNomOrd [((a, b), count (show a, show b) [(c,d) | (_,c,d) <- var]) | a <- sc1, b <- sc2]))
--
instance ScalaBi ScalaNomNum where
  conteggioBi (MakeScalaNom sc1, MakeScalaNum sc2) var = Left (MyRight (MakeDistNomNum [((a, b), count (show a, show b) [(c,d) | (_,c,d) <- var]) | a <- sc1, b <- sc2]))
--
instance ScalaBi ScalaOrdOrd where
  conteggioBi (MakeScalaOrd sc1, MakeScalaOrd sc2) var = Right (MyLeft (MakeDistOrdOrd [((a, b), count (show a, show b) [(c,d) | (_,c,d) <- var]) | a <- sc1, b <- sc2]))
--
instance ScalaBi ScalaOrdNum where
  conteggioBi (MakeScalaOrd sc1, MakeScalaNum sc2) var = Right (MyMiddle (MakeDistOrdNum [((a, b), count (show a, show b) [(c,d) | (_,c,d) <- var]) | a <- sc1, b <- sc2]))
--
instance ScalaBi ScalaNumNum where
  conteggioBi (MakeScalaNum sc1, MakeScalaNum sc2) var = Right (MyRight (MakeDistNumNum [((a, b), count (show a, show b) [(c,d) | (_,c,d) <- var]) | a <- sc1, b <- sc2]))

--------------------------------------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE CONGIUNTE BIVARIATE, DISTRIBUZIONI CONDIZIONATE e INDICE DI CONNESSIONE - CHI --
--------------------------------------------------------------------------------------------------------------

data DistNomNom = forall modalità1 modalità2. (Show modalità1, Eq modalità1, Show modalità2, Eq modalità2) => MakeDistNomNom [((modalità1, modalità2), Int)]
data DistNomOrd = forall modalità1 modalità2. (Show modalità1, Eq modalità1, Show modalità2, Eq modalità2, Ord modalità2) => MakeDistNomOrd [((modalità1, modalità2), Int)]
data DistNomNum = forall modalità1 modalità2. (Show modalità1, Eq modalità1, Show modalità2, Eq modalità2, Ord modalità2, Real modalità2) => MakeDistNomNum [((modalità1, modalità2), Int)]
data DistOrdOrd = forall modalità1 modalità2. (Show modalità1, Eq modalità1, Ord modalità1, Show modalità2, Eq modalità2, Ord modalità2) => MakeDistOrdOrd [((modalità1, modalità2), Int)]
data DistOrdNum = forall modalità1 modalità2. (Show modalità1, Eq modalità1, Ord modalità1, Show modalità2, Eq modalità2, Ord modalità2, Real modalità2) => MakeDistOrdNum [((modalità1, modalità2), Int)]
data DistNumNum = forall modalità1 modalità2. (Show modalità1, Eq modalità1, Ord modalità1, Real modalità1, Show modalità2, Eq modalità2, Ord modalità2, Real modalità2) => MakeDistNumNum [((modalità1, modalità2), Int)]

type DistBi' = Either (MyEither DistNomNom DistNomOrd DistNomNum) (MyEither DistOrdOrd DistOrdNum DistNumNum)


data DistCondNomNom = forall modalità. (Show modalità, Eq modalità) => MakeDistCondNomNom (DistAssNom, modalità)
data DistCondNomOrd = forall modalità. (Show modalità, Eq modalità, Ord modalità) => MakeDistCondNomOrd (DistAssNom, modalità)
data DistCondNomNum = forall modalità. (Show modalità, Eq modalità, Ord modalità, Real modalità) => MakeDistCondNomNum (DistAssNom, modalità)
--
data DistCondOrdNom = forall modalità. (Show modalità, Eq modalità) => MakeDistCondOrdNom (DistAssOrd, modalità)
data DistCondOrdOrd = forall modalità. (Show modalità, Eq modalità, Ord modalità) => MakeDistCondOrdOrd (DistAssOrd, modalità)
data DistCondOrdNum = forall modalità. (Show modalità, Eq modalità, Ord modalità, Real modalità) => MakeDistCondOrdNum (DistAssOrd, modalità)
--
data DistCondNumNom = forall modalità. (Show modalità, Eq modalità) => MakeDistCondNumNom (DistAssNum, modalità)
data DistCondNumOrd = forall modalità. (Show modalità, Eq modalità, Ord modalità) => MakeDistCondNumOrd (DistAssNum, modalità)
data DistCondNumNum = forall modalità. (Show modalità, Eq modalità, Ord modalità, Real modalità) => MakeDistCondNumNum (DistAssNum, modalità)

type DistCond = MyEither (MyEither [DistCondNomNom] [DistCondNomOrd] [DistCondNomNum]) (MyEither [DistCondOrdNom] [DistCondOrdOrd] [DistCondOrdNum]) (MyEither [DistCondNumNom] [DistCondNumOrd] [DistCondNumNum])

type ChiQuadro = Float
type ChiQuadroMax = Int
type ChiQuadroNorm = Float

type FrequenzeOsservate = Int
type FrequenzeTeoriche = Float
type Contingenza = [(FrequenzeOsservate, FrequenzeTeoriche)]

type Connessione = (ChiQuadro, ChiQuadroNorm)
type Dipendenza = Maybe (EtaQuadro, EtaQuadro)
type Correlazione = Maybe (Rho, RhoQuadro)

type IndiciBi = (Connessione, Dipendenza, Correlazione)

-- funzione che rimuove da una lista le ricorrenze multiple degli elementi in essa contenuti
nub :: (Eq a) => [a] -> [a]
nub l                 = nub' l []
  where
    nub' [] _         = []
    nub' (x:xs) ls
        | x `elem` ls = nub' xs ls
        | otherwise   = x : nub' xs (x:ls)


data Selezione = Uno | Due

instance Eq Selezione where
  Uno == Uno = True
  Due == Due = True
  Uno == Due = False
  Due == Uno = False

class DistBi a where
  marginalizzazione :: Selezione -> a -> DistAss'
  marginalizzazione1 :: a -> DistAss'
  marginalizzazione2 :: a -> DistAss'
  totaleBi :: a -> Int -- il totale della congiunta equivale al totale delle marginali
  totaleBi' :: a -> Int -- se il file csv è stilato correttamente, il totale delle due marginali deve coincidere
  condizionamento :: Selezione -> a -> DistCond
  condizionamento1 :: a -> DistCond
  condizionamento2 :: a -> DistCond

  contingenza :: a -> Contingenza -- appaia freq. assolute rilevate (Int) e freq. teoriche (Float)
  chiQuadro :: a -> ChiQuadro
  chiQuadroMax :: a -> ChiQuadroMax
  chiQuadroNorm :: a -> ChiQuadroNorm

  connessione :: a -> Connessione
  dipendenza :: a -> Dipendenza
  correlazione :: a -> Correlazione

  summaryBi :: a -> IndiciBi
--
instance DistBi DistNomNom where
  marginalizzazione sel (MakeDistNomNom dist) | sel == Uno  = MyLeft (MakeDistAssNom (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist]))
                                              | sel == Due  = MyLeft (MakeDistAssNom (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist]))
  marginalizzazione1 = marginalizzazione Uno
  marginalizzazione2 = marginalizzazione Due
  totaleBi dist =
     let MyLeft marg = marginalizzazione1 dist
     in  totale marg
  totaleBi' dist =
     let MyLeft marg = marginalizzazione2 dist
     in  totale marg
  condizionamento sel (MakeDistNomNom dist) | sel == Uno =
                                                let condizionanti = nub [a | ((a, _), _) <- dist]
                                                in  MyLeft (MyLeft [MakeDistCondNomNom (MakeDistAssNom [(c, d) | ((b, c), d) <- dist, b == a], a) | a <- condizionanti])
                                            | sel == Due =
                                                let condizionanti = nub [a | ((_, a), _) <- dist]
                                                in  MyLeft (MyLeft [MakeDistCondNomNom (MakeDistAssNom [(b, d) | ((b, c), d) <- dist, c == a], a) | a <- condizionanti])
  condizionamento1 = condizionamento Uno
  condizionamento2 = condizionamento Due

  contingenza (MakeDistNomNom dist) =
     let marg1 = (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist])
         marg2 = (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist])
         tot = totaleBi (MakeDistNomNom dist)
     in  [(c, (fromIntegral (e*g))/(fromIntegral tot)) | ((a,b), c) <- dist, (d, e) <- marg1, (f,g) <- marg2, a == d, b == f]
  chiQuadro dist = sum [((fromIntegral a)-b)^2/b | (a, b) <- (contingenza dist)]
  chiQuadroMax dist =
     let MyLeft marg1 = marginalizzazione1 dist
         MyLeft marg2 = marginalizzazione2 dist
         gradi1 = gradi marg1
         gradi2 = gradi marg2
         n = totaleBi dist
     in  n * (min (gradi1 - 1) (gradi2 - 1))
  chiQuadroNorm dist = (chiQuadro dist) / fromIntegral (chiQuadroMax dist)

  connessione dist = (chiQuadro dist, chiQuadroNorm dist)
  dipendenza dist = Nothing
  correlazione dist = Nothing

  summaryBi dist = (connessione dist, dipendenza dist, correlazione dist)
--
instance DistBi DistNomOrd where
  marginalizzazione sel (MakeDistNomOrd dist) | sel == Uno  = MyLeft (MakeDistAssNom (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist]))
                                              | sel == Due  = MyMiddle (MakeDistAssOrd (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist]))
  marginalizzazione1 = marginalizzazione Uno
  marginalizzazione2 = marginalizzazione Due
  totaleBi dist =
     let MyLeft marg = marginalizzazione1 dist
     in  totale marg
  totaleBi' dist =
     let MyLeft marg = marginalizzazione2 dist
     in  totale marg
  condizionamento sel (MakeDistNomOrd dist) | sel == Uno =
                                                let condizionanti = nub [a | ((a, _), _) <- dist]
                                                in  MyMiddle (MyLeft [MakeDistCondOrdNom (MakeDistAssOrd [(c, d) | ((b, c), d) <- dist, b == a], a) | a <- condizionanti])
                                            | sel == Due =
                                                let condizionanti = nub [a | ((_, a), _) <- dist]
                                                in  MyLeft (MyMiddle [MakeDistCondNomOrd (MakeDistAssNom [(b, d) | ((b, c), d) <- dist, c == a], a) | a <- condizionanti])
  condizionamento1 = condizionamento Uno
  condizionamento2 = condizionamento Due

  contingenza (MakeDistNomOrd dist) =
     let marg1 = (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist])
         marg2 = (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist])
         tot = totaleBi (MakeDistNomOrd dist)
     in  [(c, (fromIntegral (e*g))/(fromIntegral tot)) | ((a,b), c) <- dist, (d, e) <- marg1, (f,g) <- marg2, a == d, b == f]
  chiQuadro dist = sum [((fromIntegral a)-b)^2/b | (a, b) <- (contingenza dist)]
  chiQuadroMax dist =
     let MyLeft marg1 = marginalizzazione1 dist
         MyMiddle marg2 = marginalizzazione2 dist
         gradi1 = gradi marg1
         gradi2 = gradi marg2
         n = totaleBi dist
     in  n * (min (gradi1 - 1) (gradi2 - 1))
  chiQuadroNorm dist = (chiQuadro dist) / fromIntegral (chiQuadroMax dist)

  connessione dist = (chiQuadro dist, chiQuadroNorm dist)
  dipendenza dist = Nothing
  correlazione dist = Nothing

  summaryBi dist = (connessione dist, dipendenza dist, correlazione dist)
--
instance DistBi DistNomNum where
  marginalizzazione sel (MakeDistNomNum dist) | sel == Uno  = MyLeft (MakeDistAssNom (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist]))
                                              | sel == Due  = MyRight (MakeDistAssNum (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist]))
  marginalizzazione1 = marginalizzazione Uno
  marginalizzazione2 = marginalizzazione Due
  totaleBi dist =
     let MyLeft marg = marginalizzazione1 dist
     in  totale marg
  totaleBi' dist =
     let MyLeft marg = marginalizzazione2 dist
     in  totale marg
  condizionamento sel (MakeDistNomNum dist) | sel == Uno =
                                                let condizionanti = nub [a | ((a, _), _) <- dist]
                                                in  MyRight (MyLeft [MakeDistCondNumNom (MakeDistAssNum [(c, d) | ((b, c), d) <- dist, b == a], a) | a <- condizionanti])
                                            | sel == Due =
                                                let condizionanti = nub [a | ((_, a), _) <- dist]
                                                in  MyLeft (MyRight [MakeDistCondNomNum (MakeDistAssNom [(b, d) | ((b, c), d) <- dist, c == a], a) | a <- condizionanti])
  condizionamento1 = condizionamento Uno
  condizionamento2 = condizionamento Due

  contingenza (MakeDistNomNum dist) =
     let marg1 = (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist])
         marg2 = (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist])
         tot = totaleBi (MakeDistNomNum dist)
     in  [(c, (fromIntegral (e*g))/(fromIntegral tot)) | ((a,b), c) <- dist, (d, e) <- marg1, (f,g) <- marg2, a == d, b == f]
  chiQuadro dist = sum [((fromIntegral a)-b)^2/b | (a, b) <- (contingenza dist)]
  chiQuadroMax dist =
     let MyLeft marg1 = marginalizzazione1 dist
         MyRight marg2 = marginalizzazione2 dist
         gradi1 = gradi marg1
         gradi2 = gradi marg2
         n = totaleBi dist
     in  n * (min (gradi1 - 1) (gradi2 - 1))
  chiQuadroNorm dist = (chiQuadro dist) / fromIntegral (chiQuadroMax dist)

  connessione dist = (chiQuadro dist, chiQuadroNorm dist)
  dipendenza dist = Just (etaQuadro1 dist, etaQuadro2 dist)
  correlazione dist = Nothing

  summaryBi dist = (connessione dist, dipendenza dist, correlazione dist)
--
instance DistBi DistOrdOrd where
  marginalizzazione sel (MakeDistOrdOrd dist) | sel == Uno  = MyMiddle (MakeDistAssOrd (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist]))
                                              | sel == Due  = MyMiddle (MakeDistAssOrd (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist]))
  marginalizzazione1 = marginalizzazione Uno
  marginalizzazione2 = marginalizzazione Due
  totaleBi dist =
     let MyLeft marg = marginalizzazione1 dist
     in  totale marg
  totaleBi' dist =
     let MyLeft marg = marginalizzazione2 dist
     in  totale marg
  condizionamento sel (MakeDistOrdOrd dist) | sel == Uno =
                                                let condizionanti = nub [a | ((a, _), _) <- dist]
                                                in  MyMiddle (MyMiddle [MakeDistCondOrdOrd (MakeDistAssOrd [(c, d) | ((b, c), d) <- dist, b == a], a) | a <- condizionanti])
                                            | sel == Due =
                                                let condizionanti = nub [a | ((_, a), _) <- dist]
                                                in  MyMiddle (MyMiddle [MakeDistCondOrdOrd (MakeDistAssOrd [(b, d) | ((b, c), d) <- dist, c == a], a) | a <- condizionanti])
  condizionamento1 = condizionamento Uno
  condizionamento2 = condizionamento Due

  contingenza (MakeDistOrdOrd dist) =
     let marg1 = (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist])
         marg2 = (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist])
         tot = totaleBi (MakeDistOrdOrd dist)
     in  [(c, (fromIntegral (e*g))/(fromIntegral tot)) | ((a,b), c) <- dist, (d, e) <- marg1, (f,g) <- marg2, a == d, b == f]
  chiQuadro dist = sum [((fromIntegral a)-b)^2/b | (a, b) <- (contingenza dist)]
  chiQuadroMax dist =
     let MyMiddle marg1 = marginalizzazione1 dist
         MyMiddle marg2 = marginalizzazione2 dist
         gradi1 = gradi marg1
         gradi2 = gradi marg2
         n = totaleBi dist
     in  n * (min (gradi1 - 1) (gradi2 - 1))
  chiQuadroNorm dist = (chiQuadro dist) / fromIntegral (chiQuadroMax dist)

  connessione dist = (chiQuadro dist, chiQuadroNorm dist)
  dipendenza dist = Nothing
  correlazione dist = Nothing

  summaryBi dist = (connessione dist, dipendenza dist, correlazione dist)
--
instance DistBi DistOrdNum where
  marginalizzazione sel (MakeDistOrdNum dist) | sel == Uno = MyMiddle (MakeDistAssOrd (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist]))
                                              | sel == Due = MyRight (MakeDistAssNum (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist]))
  marginalizzazione1 = marginalizzazione Uno
  marginalizzazione2 = marginalizzazione Due
  totaleBi dist =
     let MyLeft marg = marginalizzazione1 dist
     in  totale marg
  totaleBi' dist =
     let MyLeft marg = marginalizzazione2 dist
     in  totale marg
  condizionamento sel (MakeDistOrdNum dist) | sel == Uno =
                                                let condizionanti = nub [a | ((a, _), _) <- dist]
                                                in  MyRight (MyMiddle [MakeDistCondNumOrd (MakeDistAssNum [(c, d) | ((b, c), d) <- dist, b == a], a) | a <- condizionanti])
                                            | sel == Due =
                                                let condizionanti = nub [a | ((_, a), _) <- dist]
                                                in  MyMiddle (MyRight [MakeDistCondOrdNum (MakeDistAssOrd [(b, d) | ((b, c), d) <- dist, c == a], a) | a <- condizionanti])
  condizionamento1 = condizionamento Uno
  condizionamento2 = condizionamento Due

  contingenza (MakeDistOrdNum dist) =
     let marg1 = (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist])
         marg2 = (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist])
         tot = totaleBi (MakeDistOrdNum dist)
     in  [(c, (fromIntegral (e*g))/(fromIntegral tot)) | ((a,b), c) <- dist, (d, e) <- marg1, (f,g) <- marg2, a == d, b == f]
  chiQuadro dist = sum [((fromIntegral a)-b)^2/b | (a, b) <- (contingenza dist)]
  chiQuadroMax dist =
     let MyMiddle marg1 = marginalizzazione1 dist
         MyRight marg2 = marginalizzazione2 dist
         gradi1 = gradi marg1
         gradi2 = gradi marg2
         n = totaleBi dist
     in  n * (min (gradi1 - 1) (gradi2 - 1))
  chiQuadroNorm dist = (chiQuadro dist) / fromIntegral (chiQuadroMax dist)

  connessione dist = (chiQuadro dist, chiQuadroNorm dist)
  dipendenza dist = Just (etaQuadro1 dist, etaQuadro2 dist)
  correlazione dist = Nothing

  summaryBi dist = (connessione dist, dipendenza dist, correlazione dist)
--
instance DistBi DistNumNum where
  marginalizzazione sel (MakeDistNumNum dist) | sel == Uno  = MyRight (MakeDistAssNum (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist]))
                                              | sel == Due  = MyRight (MakeDistAssNum (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist]))
  marginalizzazione1 = marginalizzazione Uno
  marginalizzazione2 = marginalizzazione Due
  totaleBi dist =
     let MyLeft marg = marginalizzazione1 dist
     in  totale marg
  totaleBi' dist =
     let MyLeft marg = marginalizzazione2 dist
     in  totale marg
  condizionamento sel (MakeDistNumNum dist) | sel == Uno =
                                                let condizionanti = nub [a | ((a, _), _) <- dist]
                                                in  MyRight (MyRight [MakeDistCondNumNum (MakeDistAssNum [(c, d) | ((b, c), d) <- dist, b == a], a) | a <- condizionanti])
                                            | sel == Due =
                                                let condizionanti = nub [a | ((_, a), _) <- dist]
                                                in  MyRight (MyRight [MakeDistCondNumNum (MakeDistAssNum [(b, d) | ((b, c), d) <- dist, c == a], a) | a <- condizionanti])
  condizionamento1 = condizionamento Uno
  condizionamento2 = condizionamento Due

  contingenza (MakeDistNumNum dist) =
     let marg1 = (nub [(a, sum [c | ((b, _), c) <- dist, b == a]) | ((a, _), _) <- dist])
         marg2 = (nub [(a, sum [c | ((_, b), c) <- dist, b == a]) | ((_, a), _) <- dist])
         tot = totaleBi (MakeDistNumNum dist)
     in  [(c, (fromIntegral (e*g))/(fromIntegral tot)) | ((a,b), c) <- dist, (d, e) <- marg1, (f,g) <- marg2, a == d, b == f]
  chiQuadro dist = sum [((fromIntegral a)-b)^2/b | (a, b) <- (contingenza dist)]
  chiQuadroMax dist =
     let MyRight marg1 = marginalizzazione1 dist
         MyRight marg2 = marginalizzazione2 dist
         gradi1 = gradi marg1
         gradi2 = gradi marg2
         n = totaleBi dist
     in  n * (min (gradi1 - 1) (gradi2 - 1))
  chiQuadroNorm dist = (chiQuadro dist) / fromIntegral (chiQuadroMax dist)

  connessione dist = (chiQuadro dist, chiQuadroNorm dist)
  dipendenza dist = Just (etaQuadro1 dist, etaQuadro2 dist)
  correlazione dist = Just (rho dist, rhoQuadro dist)

  summaryBi dist = (connessione dist, dipendenza dist, correlazione dist)

----------------------------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE CONGIUNTE BIVARIATE NUMERICHE e INDICE DI DIPENDENZA IN MEDIA - ETA --
----------------------------------------------------------------------------------------------------

type EtaQuadro = Maybe Float
type Devianza' = Maybe Devianza
type DevBet = Devianza'
type DevWit = Devianza'

class DistBiNum a where
  devBet :: Selezione -> a -> DevBet -- funzione che restituisce la devianza tra gruppi a.k.a. "devianza between"
  devBet1 :: a -> DevBet
  devBet2 :: a -> DevBet
  devWit :: Selezione -> a -> DevWit -- funzione che restituisce la devianza nei gruppi a.k.a. "devianza within"
  devWit1 :: a -> DevWit
  devWit2 :: a -> DevWit

  etaQuadro :: Selezione -> a -> EtaQuadro
  etaQuadro1 :: a -> EtaQuadro
  etaQuadro2 :: a -> EtaQuadro
--
instance DistBiNum DistNomNum where
  devBet sel dist | sel == Uno = Nothing
                  | sel == Due =
                      let MyRight margCondizionata = marginalizzazione2 dist
                          mu = mediaArit margCondizionata
                          MyRight (MyLeft distCondizionate) = condizionamento1 dist
                          medieCondizionate = [(mediaArit a, totale a) | (MakeDistCondNumNom (a, _)) <- distCondizionate]
                      in  Just (sum [((a - mu)^2)*(fromIntegral b) | (a, b) <- medieCondizionate])
  devBet1 = devBet Uno
  devBet2 = devBet Due
  devWit sel dist | sel == Uno = Nothing
                  | sel == Due =
                      let Just devBetween = devBet2 dist
                          MyRight margCondizionata = marginalizzazione2 dist
                          devTotale = devianza margCondizionata
                      in  Just (devTotale - devBetween)
  devWit1 = devWit Uno
  devWit2 = devWit Due

  etaQuadro sel dist | sel == Uno = Nothing
                     | sel == Due =
                         let Just devBetween = devBet2 dist
                             MyRight margCondizionata = marginalizzazione2 dist
                             devTotale = devianza margCondizionata
                         in  Just (devBetween/devTotale)
  etaQuadro1 = etaQuadro Uno
  etaQuadro2 = etaQuadro Due
--
instance DistBiNum DistOrdNum where
  devBet sel dist | sel == Uno = Nothing
                  | sel == Due =
                      let MyRight margCondizionata = marginalizzazione2 dist
                          mu = mediaArit margCondizionata
                          MyRight (MyMiddle distCondizionate) = condizionamento1 dist
                          medieCondizionate = [(mediaArit a, totale a) | (MakeDistCondNumOrd (a, _)) <- distCondizionate]
                      in  Just (sum [((a - mu)^2)*(fromIntegral b) | (a, b) <- medieCondizionate])
  devBet1 = devBet Uno
  devBet2 = devBet Due
  devWit sel dist | sel == Uno = Nothing
                  | sel == Due =
                      let Just devBetween = devBet2 dist
                          MyRight margCondizionata = marginalizzazione2 dist
                          devTotale = devianza margCondizionata
                      in  Just (devTotale - devBetween)
  devWit1 = devWit Uno
  devWit2 = devWit Due

  etaQuadro sel dist | sel == Uno = Nothing
                     | sel == Due =
                         let Just devBetween = devBet2 dist
                             MyRight margCondizionata = marginalizzazione2 dist
                             devTotale = devianza margCondizionata
                         in  Just (devBetween/devTotale)
  etaQuadro1 = etaQuadro Uno
  etaQuadro2 = etaQuadro Due
--
instance DistBiNum DistNumNum where
  devBet sel dist | sel == Uno =
                      let MyRight margCondizionata = marginalizzazione1 dist
                          mu = mediaArit margCondizionata
                          MyRight (MyRight distCondizionate) = condizionamento2 dist
                          medieCondizionate = [(mediaArit a, totale a) | (MakeDistCondNumNum (a, _)) <- distCondizionate]
                      in  Just (sum [((a - mu)^2)*(fromIntegral b) | (a, b) <- medieCondizionate])
                  | sel == Due =
                      let MyRight margCondizionata = marginalizzazione2 dist
                          mu = mediaArit margCondizionata
                          MyRight (MyRight distCondizionate) = condizionamento1 dist
                          medieCondizionate = [(mediaArit a, totale a) | (MakeDistCondNumNum (a, _)) <- distCondizionate]
                      in  Just (sum [((a - mu)^2)*(fromIntegral b) | (a, b) <- medieCondizionate])
  devBet1 = devBet Uno
  devBet2 = devBet Due
  devWit sel dist | sel == Uno =
                      let Just devBetween = devBet1 dist
                          MyRight margCondizionata = marginalizzazione1 dist
                          devTotale = devianza margCondizionata
                      in  Just (devTotale - devBetween)
                  | sel == Due =
                      let Just devBetween = devBet2 dist
                          MyRight margCondizionata = marginalizzazione2 dist
                          devTotale = devianza margCondizionata
                      in  Just (devTotale - devBetween)
  devWit1 = devWit Uno
  devWit2 = devWit Due

  etaQuadro sel dist | sel == Uno =
                         let Just devBetween = devBet1 dist
                             MyRight margCondizionata = marginalizzazione1 dist
                             devTotale = devianza margCondizionata
                         in  Just (devBetween/devTotale)
                     | sel == Due =
                         let Just devBetween = devBet2 dist
                             MyRight margCondizionata = marginalizzazione2 dist
                             devTotale = devianza margCondizionata
                         in  Just (devBetween/devTotale)
  etaQuadro1 = etaQuadro Uno
  etaQuadro2 = etaQuadro Due

--------------------------------------------------------------------------------
-- DISTRIBUZIONI DI FREQUENZE CONGIUNTE BIVARIATE NUMERICHE "DOPPIE"          --
--------------------------------------------------------------------------------

type Codevianza = Devianza
type Covarianza = Varianza


codevianza :: DistNumNum -> Codevianza
codevianza (MakeDistNumNum dist) =
   let MyRight marg1 = marginalizzazione1 (MakeDistNumNum dist)
       MyRight marg2 = marginalizzazione2 (MakeDistNumNum dist)
       mu1 = mediaArit marg1
       mu2 = mediaArit marg2
   in  sum [((realToFrac a) - mu1)*((realToFrac b) - mu2)*(fromIntegral c) | ((a, b), c) <- dist]

covarianza :: DistNumNum -> Covarianza
covarianza (MakeDistNumNum dist) =
   let MyRight marg1 = marginalizzazione1 (MakeDistNumNum dist)
       MyRight marg2 = marginalizzazione2 (MakeDistNumNum dist)
       mu1 = mediaArit marg1
       mu2 = mediaArit marg2
   in  (sum [((realToFrac a) - mu1)*((realToFrac b) - mu2)*(fromIntegral c) | ((a, b), c) <- dist])/(fromIntegral (totaleBi (MakeDistNumNum dist)))

covarianza' :: DistNumNum -> Covarianza -- definizione alternativa della covarianza come codevianza/n
covarianza' dist = (codevianza dist)/(fromIntegral (totaleBi dist))

--------------------------------------------------------------------------------
-- COEFFICIENTE DI CORRELAZIONE LINEARE - RHO                                 --
--------------------------------------------------------------------------------

type Rho = Float
type RhoQuadro = Float

rho :: DistNumNum -> Rho
rho dist =
   let cov = covarianza dist
       MyRight marg1 = marginalizzazione1 dist
       MyRight marg2 = marginalizzazione2 dist
       sqm1 = sqm marg1
       sqm2 = sqm marg2
   in  cov/(sqm1*sqm2)

rho' :: DistNumNum -> Rho
rho' dist =
   let codev = codevianza dist
       MyRight marg1 = marginalizzazione1 dist
       MyRight marg2 = marginalizzazione2 dist
       dev1 = devianza marg1
       dev2 = devianza marg2
   in  codev/(dev1*dev2)

rhoQuadro :: DistNumNum -> RhoQuadro
rhoQuadro dist = (rho dist)^2

--------------------------------------------------------------------------------
-- REGRESSIONE LINEARE SEMPLICE                                               --
--------------------------------------------------------------------------------

-- retta di regressione:
--
--     y = q + bx + e

type Intercetta = Float -- q
type Parametro = Float -- b
type RQuadro = Float -- l'indice di adattamento, che in questo caso coincide con il RhoQuadro


data Regressione = MakeReg DistAssNum DistAssNum Intercetta Parametro RQuadro

regressione :: Selezione -> DistNumNum -> Regressione
regressione sel dist | sel == Uno =
                         let MyRight y = marginalizzazione1 dist
                             MyRight x = marginalizzazione2 dist
                             muy = mediaArit y
                             mux = mediaArit x
                             varx = varianza x
                             cov = covarianza dist
                             r = rhoQuadro dist
                             b = cov/varx
                             q = muy - (b*mux)
                         in  (MakeReg y x q b r)
                     | sel == Due =
                         let MyRight y = marginalizzazione2 dist
                             MyRight x = marginalizzazione1 dist
                             muy = mediaArit y
                             mux = mediaArit x
                             varx = varianza x
                             cov = covarianza dist
                             r = rhoQuadro dist
                             b = cov/varx
                             q = muy - (b*mux)
                         in  (MakeReg y x q b r)
