multi :: [[Char]] -> [[Char]] -> Int
multi xs ys = if tot == 0 then 0 else 2^(tot - 1)
    where tot = sum . map (\x -> if x `elem` ys then 1 else 0) $ xs

