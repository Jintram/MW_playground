utf8ToInt('a')-96
intToUtf8(10+96)
intToUtf8(9+96)
intToUtf8(21+96)

sapply(c(5, 22, 4)+96, intToUtf8)
sapply(c(7, 22, 4)+96, intToUtf8)

sapply(c(9, 22, 1, 14, 8, 1, 
         25, 12, 5, 25)+96, intToUtf8)
