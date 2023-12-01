import System.IO
import Data.Char

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let converted = map nums . lines $ contents
    let value = show . sum . map calVal $ converted
    putStr ( value ++ "\n")
    hClose handle

calVal x = read (head digits:last digits:[]) :: Int
    where digits = filter isDigit x

nums :: [Char] -> [Char]
nums "" = ""
nums ('o':'n':'e':xs) = "1" ++ nums ('e':xs)
nums ('t':'w':'o':xs) = "2" ++ nums ('o':xs)
nums ('t':'h':'r':'e':'e':xs) = "3" ++ nums ('e':xs)
nums ('f':'o':'u':'r':xs) = "4" ++ nums ('r':xs)
nums ('f':'i':'v':'e':xs) = "5" ++ nums ('e':xs)
nums ('s':'i':'x':xs) = "6" ++ nums ('x':xs)
nums ('s':'e':'v':'e':'n':xs) = "7" ++ nums ('n':xs)
nums ('e':'i':'g':'h':'t':xs) = "8" ++ nums ('t':xs)
nums ('n':'i':'n':'e':xs) = "9" ++ nums ('e':xs)
nums (x:xs) = x:[] ++ nums xs
