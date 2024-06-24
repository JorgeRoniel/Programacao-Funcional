dobra :: Int -> Int
dobra n = n + n

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista (prim:outros) = (dobra prim) : (dobraLista outros)

mapInt :: (Int -> Int) -> [Int] -> [Int]
mapInt _ [] = []
mapInt f (prim:outros) = (f prim) : (mapInt f outros)