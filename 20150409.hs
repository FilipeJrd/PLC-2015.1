



data Aresta = NulA | Aresta ( Int , Int )  deriving(Eq , Show)
data Node = NulN |Node (Int ,[Aresta]) deriving(Eq , Show)
data Grafo = Nul | Grafo [ Node ]  deriving(Eq , Show)


nodes :: [Node]
nodes = [(Node (1,[(Aresta (2,1))])),(Node (2,[ (Aresta(1,1))] )) ]

contains :: [ Node ] -> Node -> Bool
contains [] _ = False
contains a l | head a == l = True
			 | otherwise = contains (tail a) l


dfsAux :: [(Grafo , Int)]-> [Int] -> Int-> Bool
dfsAux [] _ _= False
dfsAux ((g,_):xs) c e = (dfs g c e) || dfsAux xs c e
 
dfs ::Grafo-> [Int]  -> Int -> Bool
 
dfs (Node a l) c e | elem a c = False
                   | a == e = True
                   | otherwise = dfsAux l (a:c) e


dfs :: Grafo -> [Node] -> Int -> Bool
dfs ( Grafo( Node (a,b) ): xs) ((Node (c,d)) : xt) e | contains ((Node (c,d)) : xt) (Node (a,b)) = False
                   									 | a == e = True
                                                     | otherwise = dfsAux 
