module Utils where

fileNameWithoutExtension fname =
  foldl1 (\x y -> x++"."++y) $ reverse $ snd $ foldl (\(y,res) x-> if x == '.' then ("",[y] ++ res) else (y++[x],res)) ("",[]) fname
