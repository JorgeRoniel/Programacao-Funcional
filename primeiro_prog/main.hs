module Main where

-- FUNÇÕES BÁSICAS

-- Calcula o polinomio de um numero
polinomio :: Int -> Int
polinomio x = x*x + 10*x + 2

-- Calcula o quadrado de um numero
quadrado :: Int -> Int
quadrado n = n * n


-- Calcula o perimetro de um circulo
perimetro :: Float -> Float
perimetro r = 2*r*pi

-- CONDICIONAIS

-- Verifica se o numero é par
paridade :: Int -> Bool
paridade x 
    | mod x 2 == 0 = True
    | mod x 2 == 1 = False

-- Verifica se o caracter digitado é maiusculo ou minusculo
charcase :: Char -> String
charcase c = if c >= 'a' && c <= 'z'
    then "Minuscula"
    else if c >= 'A' && c <= 'Z'
        then "Maiuscula"
        else "Desconhecido"

condicoes :: Int -> Int -> Int -> Int
condicoes a b c
    | a == 0 = b*b + c*3
    | a == 1 = 2* c^2 - c*3
    | a == 2 = 3*c - b^2 

-- WHERE

baskhara :: Int -> Int -> Int -> Int
baskhara a b c 
        | d > 0 = 2
        | d < 0 = 0
        | d == 0 = 1
    where
        d = b^2 - 4*a*c

-- RECURSÃO

resto :: Int -> Int -> Int
resto a b 
    | b > a = a
    | b == a = 0
    | otherwise = resto (a-b) b

fatorial :: Int -> Int
fatorial x 
    | x == 0 = 1
    | x > 0 = x * fatorial (x-1)

mult :: Int -> Int -> Int
mult a b
    | a == 1 = b
    | b > 1 = b + mult (a - 1) b

main :: IO()
main = do
    let a = 10
    let b = 10
    let c = 3
    let res = mult a b
    putStr ("É " ++ show res)