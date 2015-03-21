double :: [Int] -> [Int]
double e | e == [] = []
         | (tail e) == [] = [2*(head e)]
         | otherwise = [2*(head e)] ++ double (tail e)


member :: [Int] -> Int -> Bool
member l m | l == [] = False
           | (head l) == m = True
           | otherwise = member (tail l) m

filtrar :: String -> String
filtrar s | s == [] = []
          | (head s >= '0') && (head s <= '9') = [(head s)] ++ filtrar (tail s)
          | otherwise = filtrar (tail s)

sumpairs :: [Int] -> [Int] -> [Int]
sumpairs f s | f == [] = []
             | s == [] = []
             | otherwise = [(head f) + (head s)] ++ sumpairs (tail f) (tail s)



--funcoes usadas no quick sort

bigger :: Int -> [Int] -> [Int]
bigger p l | l == [] = []
           | (head l) >= p = [head l] ++ bigger  p (tail l)
           | otherwise = bigger  p (tail l)


smaller :: Int -> [Int] -> [Int]
smaller p l | l == [] = []
            | (head l) < p = [head l] ++ smaller p (tail l)
            | otherwise = smaller p (tail l)


quicksort :: [Int] -> [Int]
quicksort l | l == [] = []
			| otherwise = quicksort (smaller (head l) (tail l)) ++ [head l] ++ quicksort (bigger (head l) (tail l)) 
