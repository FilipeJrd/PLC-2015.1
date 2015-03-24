merge :: Ord a =>[a] -> [a] -> [a]
merge  xs []         = xs -- se uma das listas estiver vazia retorna a outra
merge  [] ys         = ys
merge  (x:xs) (y:ys)--duas listas não vazias
    | (x <= y) = x: merge xs (y:ys)--coloca o menor na frente e continua ordenando os outros
    | otherwise = y: merge (x:xs) ys

split :: [a] -> ([a],[a])   
split (x:y:z) = (x:x1,y:y1) where (x1,y1) = split z
split xs       = (xs,[]) 


mergesort :: (Ord a) =>  [a] -> [a]
mergesort  []   = []
mergesort  [x]  = [x]
mergesort  xs = merge  (mergesort  xs1) (mergesort  xs2)
  where
    (xs1,xs2) = split xs
