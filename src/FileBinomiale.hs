module FileBinomiale where

data TBinomial a = TEmpty | Node a Int [TBinomial a] deriving (Read, Eq)
type FBinomiale a = [TBinomial a]

-- instance Show a => Show (TBinomial a) where
  -- show TEmpty = ""
  -- show (Node ele i [] ) = show ele
  -- show (Node ele i li) = show ele ++ show li 

instance Show a => Show (TBinomial a) where
  show bt =  show $ traverseBF bt

traverseBF :: TBinomial a -> [a]
traverseBF TEmpty = error "Empty Binomial Tree!"
traverseBF bt = tbf [bt]
  where
    tbf [] = []
    tbf xs = map racine xs ++ tbf (concatMap decapiter xs)

estTVide :: TBinomial a -> Bool
estTVide TEmpty = True
estTVide _ = False

degre :: TBinomial a -> Int
degre TEmpty = 0
degre (Node _ deg li) = deg

racine :: TBinomial a -> a
racine (Node val _ _) = val

union2Tid :: (Ord a) => TBinomial a -> TBinomial a -> TBinomial a
union2Tid (Node val1 deg1 li1) (Node val2 deg2 li2)
  | deg1 /= deg2 = undefined
  | val1 < val2 = (Node val1 (deg1+1) ((Node val2 deg1 li2):li1)) 
  | otherwise = (Node val2 (deg1+1) ((Node val1 deg1 li1):li2)) 
union2Tid TEmpty tree = tree
union2Tid tree TEmpty = tree

decapiter :: TBinomial a -> FBinomiale a
decapiter TEmpty = []
decapiter (Node _ _ children) = children

-- File binomiales

-- Operations primitives

file :: TBinomial a -> FBinomiale a
file tree = [tree]

estFVide :: FBinomiale a -> Bool
estFVide [] = True
estFVide _ = False

minDeg :: FBinomiale a -> TBinomial a
minDeg (tb:tail) = tb

-- Retourne le reste de la file binomiale (ie: enleve le tournoi de degre le plus petit)
reste :: FBinomiale a -> FBinomiale a
reste (_:li) = li

ajoutMin :: FBinomiale a -> TBinomial a -> FBinomiale a
ajoutMin li TEmpty = li
ajoutMin li tb = (tb:li)

-- Methodes

uFret :: (Ord a) => FBinomiale a -> FBinomiale a -> TBinomial a -> FBinomiale a
uFret [] fb2 TEmpty = fb2
uFret [] fb2 t = uFret fb2 (file t) TEmpty
uFret fb1 [] TEmpty = fb1
uFret fb1 [] t = uFret fb1 (file t) TEmpty
uFret fb1 fb2 TEmpty
  | (degre t1) < (degre t2) = (ajoutMin (union (reste fb1) fb2) t1)
  | degre(t1) > (degre t2) = (ajoutMin (union (reste fb2) fb1) t2)
  | otherwise = (uFret (reste fb1) (reste fb2) (union2Tid t1 t2))
  where (t1, t2) = ((minDeg fb1), (minDeg fb2))
uFret fb1 fb2 t 
  | ((degre t) < (degre t1)) && ((degre t) < (degre t2)) = (ajoutMin (union fb1 fb2) t)
  | ((degre t) == (degre t1)) && ((degre t) == (degre t2)) = (ajoutMin (uFret (reste fb1) (reste fb2) (union2Tid t1 t2)) t)
  | ((degre t) == (degre t1)) && ((degre t) < (degre t2)) = (uFret (reste fb1) fb2 (union2Tid t1 t))
  | ((degre t) == (degre t2)) && ((degre t) < (degre t1)) = (uFret (reste fb2) fb1 (union2Tid t2 t))
  where (t1, t2) = ((minDeg fb1), (minDeg fb2))

union :: (Ord a) => FBinomiale a -> FBinomiale a -> FBinomiale a
union fb1 fb2 = uFret fb1 fb2 TEmpty

ajoutTB :: (Ord a) => FBinomiale a -> TBinomial a -> FBinomiale a
ajoutTB fb tb = union fb [tb]

ajoutElem :: (Ord a) => FBinomiale a -> a -> FBinomiale a
ajoutElem fb elem = (ajoutTB fb (Node elem 0 []))

consIterAux :: (Ord a) => [a] -> FBinomiale a -> FBinomiale a
consIterAux [] fb = fb
consIterAux (e:li) fb = consIterAux li (ajoutElem fb e)

consIter :: (Ord a) => [a] -> FBinomiale a
consIter li = consIterAux li []

findMinAux :: (Ord a) => FBinomiale a -> TBinomial a -> TBinomial a
findMinAux [] tb = tb
findMinAux (tb1@(Node val1 _ _):tail) (tb2@(Node val2 _ _))
  | val1 < val2 = findMinAux tail tb1
  | otherwise = (findMinAux tail tb2)

findMin :: (Ord a) => FBinomiale a -> TBinomial a
findMin (head:tail) = (findMinAux tail head)

supprTB :: FBinomiale a -> TBinomial a -> FBinomiale a
supprTB [] tb = []
supprTB (tb1@(Node _ deg1 _):tail) (tb2@(Node _ deg2 _))
  | deg1 == deg2 = tail
  | otherwise = (tb1:(supprTB tail tb2))

supprMin :: (Ord a) => FBinomiale a -> FBinomiale a
supprMin [] = []
supprMin (head:tail) = (union (supprTB (head:tail) tbmin) (decapiter tbmin))
  where tbmin = (findMinAux tail head)
