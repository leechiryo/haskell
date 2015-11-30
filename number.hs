number :: String -> String
number "1000" = "onethousand"
number "0" = ""
number "1" = "one"
number "2" = "two"
number "3" = "three"
number "4" = "four"
number "5" = "five"
number "6" = "six"
number "7" = "seven"
number "8" = "eight"
number "9" = "nine"
number "10" = "ten"
number "11" = "eleven"
number "12" = "twelve"
number "13" = "thirteen"
number "14" = "fourteen"
number "15" = "fifteen"
number "16" = "sixteen"
number "17" = "seventeen"
number "18" = "eighteen"
number "19" = "nineteen"
number ('2':[a]) = "twenty" ++ number (a:[])
number ('3':[a]) = "thirty" ++ number (a:[])
number ('4':[a]) = "forty" ++ number (a:[])
number ('5':[a]) = "fifty" ++ number (a:[])
number ('6':[a]) = "sixty" ++ number (a:[])
number ('7':[a]) = "seventy" ++ number (a:[])
number ('8':[a]) = "eighty" ++ number (a:[])
number ('9':[a]) = "ninety" ++ number (a:[])
number ('0':[a]) = number (a:[])
number (a:"00") = number (a:[]) ++ "hundred"
number [a,b,c] = number (a:[]) ++ "hundredand" ++ number [b,c]