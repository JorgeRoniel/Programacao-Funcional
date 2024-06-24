import Data.Char (toLower)
import Data.List (delete)
 
vogal :: Char -> Bool
vogal c = elem (toLower c) "aeiou"  
 
consoante :: Char -> Bool
consoante c = not (vogal c)
 
validaSenha :: String -> Bool
validaSenha senha =
    let senhaMin = map toLower senha  
        qtdeVogais = length $ filter vogal senhaMin --Conta o número de vogais
        qtdeConsoantes = length $ filter consoante senhaMin --Conta o número de consoantes
        tamSenha = length senha -- Conta o número de letras
    in tamSenha >= 4 && tamSenha <= 8 && qtdeVogais >= 2 && qtdeConsoantes >= 2

listaSenhas :: (String -> Bool) -> [String] -> [String]
listaSenhas _ [] = []
listaSenhas f (s: ss)
    | f s = s : listaSenhas f ss
    | otherwise = listaSenhas f ss
