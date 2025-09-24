short :: [a] -> Bool
short l
  | length l <= 2 = True
  | otherwise = False

short' :: [a] -> Bool
short' l = length l <= 2

short''' :: [a] -> Bool
short''' [] = True
short''' [_] = True
short''' [_,_] = True
short''' _ = False