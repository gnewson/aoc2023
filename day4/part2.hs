import System.IO
import Data.Char

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    -- let games = map lineVal . map words . lines $ contents
    let games = map lineVal . lines $ contents
    let value = show . sum  $ games
    putStr ( value ++ "\n")
    hClose handle

lineVal :: [Char] -> Int
lineVal xs = wins (drop 2 x) (drop 1 y)
    where (x,y) = splitAt 12 (words xs)

wins :: [[Char]] -> [[Char]] -> Int
wins xs ys = sum . map (\x -> if x `elem` ys then 1 else 0) $ xs
