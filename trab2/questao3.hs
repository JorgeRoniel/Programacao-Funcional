type Hora = (Int,Int)
data Mensagem = Whatsapp String String Hora
    |Twitter String String Hora
    |Instagram String String Hora
        deriving (Eq,Ord,Show)

item1,item2,item3 :: Mensagem
item1 = Whatsapp "8812345678" "bom dia" (7,05)
item2 = Twitter "g1" "esta noticia nao e verdadeira" (10,42)
item3 = Instagram "tiacleide" "lindos!" (19,50)
item4 = Whatsapp "8812345678" "faz o pix" (7,05)
item5 = Twitter "neymarjr" "o ousado chegou !!!" (20,12)
item6 = Instagram "elonmusk" "nao me marca em sorteio" (10,00)
item7 = Whatsapp "8812345679" "oi sumida" (7,05)
item8 = Twitter "ex-presidente" "grande dia, ta ok?" (9,00)
item9 = Instagram "vitao" "meu casal" (3,50)

-- Questão 3
isTwitter :: Mensagem -> Bool
isTwitter (Twitter _ _ _) = True
isTwitter _ = False

mensagensTwitter :: [Mensagem] -> String
mensagensTwitter mensagens = 
    unlines [ "Mensagem: " ++ mensagem
    | Twitter _ mensagem _ <- filter isTwitter mensagens ]
        
-- Questão 4
isInstagram :: Mensagem -> Bool
isInstagram (Instagram _ _ _) = True
isInstagram _ = False

mensagensInsta :: [Mensagem] -> String
mensagensInsta mensagens = 
    unlines [ "Mensagem: " ++ mensagem
    | Instagram _ mensagem _ <- filter isInstagram mensagens ]