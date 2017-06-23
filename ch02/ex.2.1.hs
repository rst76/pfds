suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes s = s : suffixes (tail s)
