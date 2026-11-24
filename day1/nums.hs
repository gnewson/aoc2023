nums :: [Char] -> [Char]
nums "" = ""
nums ('o':'n':'e':xs) = "1" ++ nums xs
nums ('t':'w':'o':xs) = "2" ++ nums xs
nums ('t':'h':'r':'e':'e':xs) = "3" ++ nums xs
nums ('f':'o':'u':'r':xs) = "4" ++ nums xs
nums ('f':'i':'v':'e':xs) = "5" ++ nums xs
nums ('s':'i':'x':xs) = "6" ++ nums xs
nums ('s':'e':'v':'e':'n':xs) = "7" ++ nums xs
nums ('e':'i':'g':'h':'t':xs) = "8" ++ nums xs
nums ('n':'i':'n':'e':xs) = "9" ++ nums xs
nums (x:xs) = x:[] ++ nums xs
