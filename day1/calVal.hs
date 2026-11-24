import Data.Char
calVal x = read (head digits:last digits:[]) :: Int
    where digits = filter isDigit x
