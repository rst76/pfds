suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs@(_ : xs') = xs : suffixes xs'
