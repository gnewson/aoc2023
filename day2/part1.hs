import System.IO
import Data.Char

maxBlue = 14
maxRed = 12
maxGreen = 13

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let games = map gameValue . map words . lines $ contents
    let value = show . sum  $ games
    putStr ( value ++ "\n")
    hClose handle


gameValue :: [[Char]] -> Int
gameValue (_:x:xs) = if cc xs then 0 else read ( filter(`notElem` ":") x ) :: Int

cc :: [[Char]] -> Bool
cc [] = False
cc (x:y:xs) = case filter (`notElem` ",;") y of "red" -> read x > maxRed || cc xs
                                                "blue" -> read x > maxBlue || cc xs
                                                "green" -> read x > maxGreen || cc xs
gameSum :: Int -> [[Char]] -> Int
gameSum i xs = i + gameValue xs
