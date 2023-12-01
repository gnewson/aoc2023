import System.IO
import Data.Char

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let value = show . sum . map calVal . lines $ contents 
    putStr ( value ++ "\n")
    hClose handle

calVal x = read (head digits:last digits:[]) :: Int
    where digits = filter isDigit x
