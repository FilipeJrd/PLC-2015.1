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