import Data.Char

maxBlue = 14
maxRed = 12
maxGreen = 13

cc :: [[Char]] -> Bool
cc [] = False
cc (x:y:xs) = case filter (`notElem` ",;") y of "red" -> read x > maxRed || cc xs
                                                "blue" -> read x > maxBlue || cc xs
                                                "green" -> read x > maxGreen || cc xs

