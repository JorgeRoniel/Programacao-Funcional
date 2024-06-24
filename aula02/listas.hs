comprimento :: [Int] -> Int
comprimento [] = 0
comprimento (x:xs) = 1 + comprimento xs

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

checarchar :: [Char] -> Char -> Bool
checarchar [] c = False
checarchar (x:xs) c
    | x == c = True
    |otherwise = checarchar xs c

maiorNum :: [Int] -> Int
maiorNum [] = 0
maiorNum (x:xs)
        | x >= maiorCauda = x
        | otherwise = maiorCauda
    where
        maiorCauda = maiorNum xs

raizes :: Float -> Float -> Float -> [Float]
raizes a b c 
        | d < 0 = []
        | d == 0 = [(-b)/(2*a)]
        | d > 0 = [(-b) - sqrt d / (2*a), (-b) + sqrt d / (2*a)] 
    where
        d = b^2 - 4*a*c

multiplosDeN :: Int -> [Int]
multiplosDeN n = [n*x| x <- [1 .. 10]]

listaImpar :: Int -> [Int]
listaImpar n = [x| x <- [1 .. n], mod x 2 == 1]

divideLista :: [a] -> ([a], [a])
divideLista (x:xs) = splitAt n (x:xs)
    where
        comp = length (x:xs)
        n = div comp 2

funcao :: Num a => [a] -> a
funcao (x:y:_) = x + y
funcao [x] = x
funcao _ = 0

somaQuadrados :: Int -> Int -> Int
somaQuadrados n m = quadN + quadM
    where
        quad :: Int -> Int
        quad num = num * num

        quadM = quad m
        quadN = quad n