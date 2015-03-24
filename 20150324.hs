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

------------------------------------------
--aula


menor :: Int -> Int -> Int -> Int
menor a b c | (a <= b) && (a <= c) = a
			| (b <= a) && (b <= c) = b
			| otherwise = c

maior :: Int -> Int -> Int -> Int
maior a b c | (a >= b) && (a >= c) = a
			| (b >= a) && (b >= c) = b
			| otherwise = c
meio :: Int -> Int -> Int -> Int
meio a b c | ((a >= b) && (a <= c)) || ((a >= c) && (a <= b)) = a
		   | ((b >= a) && (b <= c)) || ((b >= c) && (b <= a)) = b
		   | otherwise = c

menorMaior :: Int -> Int -> Int -> (Int,Int)
menorMaior a b c = (menor a b c , maior a b c)

ordenaTripla :: (Int, Int , Int) -> (Int , Int , Int)
ordenaTripla (a,b,c) = (menor a b c, meio a b c, maior a b c)

type Ponto = (Float, Float)

type Reta = (Ponto, Ponto)


type Pessoa = String

type Livro = String

type BancoDados = [(Pessoa,Livro)]

firstCoord :: Ponto -> Float
firstCoord (x,y) = x

secondCoord :: Ponto -> Float
secondCoord (x,y) = y

vertical :: Reta -> Bool
vertical ((x1,y1),(x2,y2)) | (x1 == x2) = True
						   | otherwise = False

checkReta :: Float -> Reta -> Float
checkReta x ((x1,y1),(x2,y2))  = (((y2-y1) / (x2-x1))*(x - x1))+y1

livros :: BancoDados -> Pessoa -> [Livro]
livros b pessoa = [l | (p,l) <- b , p == pessoa]