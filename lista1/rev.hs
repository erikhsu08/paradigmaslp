rev :: [int] -> [int]
rev [] = []
rev (x:xs) = rev(xs) ++ [x]
