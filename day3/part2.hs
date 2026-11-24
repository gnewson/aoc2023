import System.IO
import Data.Char

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let games = map gameValue . map words . lines $ contents
    let value = show . sum  $ games
    putStr ( value ++ "\n")
    hClose handle


gameValue :: [[Char]] -> Int
gameValue (_:_:xs) = maxColours xs

maxColours xs = maxRed xs * maxBlue xs * maxGreen xs

-- It might be tidier to combine these 3.  An improvement for another day.
maxRed :: [[Char]] -> Int
maxRed [] = 0
maxRed (x:y:xs) = if filter (`notElem` ",;") y == "red" then max (read x) (maxRed xs) else max 0 (maxRed xs)
maxBlue :: [[Char]] -> Int
maxBlue [] = 0
maxBlue (x:y:xs) = if filter (`notElem` ",;") y == "blue" then max (read x) (maxBlue xs) else max 0 (maxBlue xs)

maxGreen :: [[Char]] -> Int
maxGreen [] = 0
maxGreen (x:y:xs) = if filter (`notElem` ",;") y == "green" then max (read x) (maxGreen xs) else max 0 (maxGreen xs)

gameSum :: Int -> [[Char]] -> Int
gameSum i xs = i + gameValue xs

