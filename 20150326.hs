tak :: [t] -> Int -> [t]
tak [] pos = []
tak a 0 = []
tak a pos = (head (a): tak (tail a) (pos-1))

dropi :: [t] -> Int -> [t]
dropi [] pos = []
dropi a pos | pos > 0 = dropi (tail a) (pos-1)
		    | otherwise = (head a : dropi(tail a) (pos-1))

takeWhilei :: (a -> Bool) -> [a] -> [a]
takeWhilei pred [] = []
takeWhilei pred list | pred (head list)  = (head list : takeWhilei pred (tail list))
					 | otherwise = []

dropWhilei :: (a -> Bool) -> [a] -> [a]
dropWhilei pred [] = []
dropWhilei pred list | pred (head list) = dropWhilei pred (tail list)
					 | otherwise = list


merge :: Ord a =>  (a -> a -> Bool) -> [a] -> [a] -> [a]
merge  pred xs []         = xs -- se uma das listas estiver vazia retorna a outra
merge  pred [] ys         = ys
merge  pred (x:xs) (y:ys)--duas listas não vazias
    | pred x y = x: merge pred xs (y:ys)--coloca o menor na frente e continua ordenando os outros
    | otherwise = y: merge pred (x:xs) ys

split :: [a] -> ([a],[a])   
split (x:y:z) = (x:x1,y:y1) where (x1,y1) = split z
split xs       = (xs,[]) 


mergesort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
mergesort  pred []   = []
mergesort  pred [x]  = [x]
mergesort  pred xs = merge  pred (mergesort pred xs1) (mergesort pred xs2)
  where
    (xs1,xs2) = split xs

conta :: [a] -> Int
conta [] = 0
conta (x:xs) = 1 + conta xs

existe :: [(a,b)] -> a -> Bool
existe [] comp = False
existe [(a,b)] comp | a

