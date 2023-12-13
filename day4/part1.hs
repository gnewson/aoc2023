import System.IO
import Data.Char

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let games = map lineVal . lines $ contents
    let value = show . sum  $ games
    putStr ( value ++ "\n")
    hClose handle

lineVal :: [Char] -> Int
lineVal xs = multi (drop 2 x) (drop 1 y)
    where (x,y) = splitAt 12 (words xs)

multi :: [[Char]] -> [[Char]] -> Int
multi xs ys = if tot == 0 then 0 else 2^(tot - 1)
    where tot = sum . map (\x -> if x `elem` ys then 1 else 0) $ xs
