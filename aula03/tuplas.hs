type Ponto = (Float, Float, Float)

dimensoes :: Ponto -> Ponto -> Float
dimensoes (x1, y1, z1) (x2, y2, z2) = sqrt (difx^2 + dify^2 + difz^2)
    where
        difx = x1 - x2
        dify = y1 - y2
        difz = z1 - z2

opp :: (Int, (Int, Int)) -> Int
opp z 
    | fst z == 1 = fst (snd z) + snd (snd z)
    | fst z == 2 = fst (snd z) - snd (snd z)
    | otherwise = 0

opp2 :: (Int, (Int, Int)) -> Int
opp2 (1, (a, b)) = a + b
opp2 (2, (a, b)) = a - b
opp2 _ = 0