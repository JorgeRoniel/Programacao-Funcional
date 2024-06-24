module Main where
import System.IO (stdout, hSetBuffering, BufferMode (NoBuffering))

media :: Float -> Float -> Float -> Float
media n1 n2 n3 = (n1 + n2 + n3) / 3

conceito :: Float -> Char
conceito media 
    | media >= 8 && media <= 10 = 'A'
    | media >= 7 && media < 8 = 'B'
    | media >= 6 && media < 7 = 'C'
    | media >= 5 && media < 6 = 'D'
    | media >= 0 && media < 5 = 'E'
    | otherwise = '-'

main :: IO()
main = do
    hSetBuffering stdout NoBuffering 
    putStrLn "Digite a primeira nota: "
    n1 <- readLn ::IO Float
    putStrLn "Digite a primeira nota: "
    n2 <- readLn ::IO Float
    putStrLn "Digite a primeira nota: "
    n3 <- readLn ::IO Float
    putStrLn "Conceito obtido: "
    let med = media n1 n2 n3
    let conc = conceito med
    putChar conc
    putStrLn ""
    