data ABR a = ABREmpty | ABRNode a (ABR a) (ABR a) deriving (Eq, Read)



instance Show a => Show (ABR a) where
  show (ABREmpty) = ""
  show (ABRNode e lchild rchild) = (show lchild) ++ ", " ++ (show e) ++ (show rchild)



isEmpty :: ABR a -> a -> Bool
isEmpty ABREmpty e = True
isEmpty _ _ = False

insertABR :: (Ord a) => ABR a -> a -> ABR a
insertABR ABREmpty e = ABRNode e ABREmpty ABREmpty
insertABR (ABRNode val lchild rchild) e
  | val < e = ABRNode val lchild (insertABR rchild e)
  | otherwise = ABRNode val (insertABR lchild e) rchild


containsABR :: (Ord a) => ABR a -> a -> Bool
containsABR ABREmpty _ = False
containsABR (ABRNode val lchild rchild) e
  | val == e = True
  | val < e = containsABR rchild e
  | otherwise = containsABR lchild e


-- Insere tous les elements d'une liste dans un ABR
insertAll :: (Ord a) => [a] -> ABR a -> ABR a
insertAll [] abr = abr
insertAll (head:tail) abr = (insertAll tail (insertABR abr head))


-- Construit un ABR a partir d'une liste d'elements
consABR :: (Ord a) => [a] -> ABR a
consABR [] = ABREmpty
consABR li = insertAll li ABREmpty
