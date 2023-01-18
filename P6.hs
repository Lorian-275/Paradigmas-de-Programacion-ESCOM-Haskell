producto :: [[a]] -> [[a]]
producto = sequence

cartesiano :: [a]->[a]->[a]->[[a]]
cartesiano x y z = sequence [x, y, z]

mapa :: [[String]]
mapa =  sequence [["","","","","","","","","","","","","","","","","","","","","","","","",""], [""], [""], [""], [""], [""], [""],[""], [""], [""], [""], [""], [""],[""], [""], [""], [""], [""], [""],[""], [""], [""], [""], [""], [""]]


imprimir :: [[String]] -> IO()
imprimir [] = print $ ' '
imprimir (x:xs) = do
  print $ x
  imprimir xs
  

cambia :: String -> (Int, Int) -> [String] -> (Int, Int) -> [String]
cambia a (b,c) (x:xs) (d,e)
	| d /= b-1 = x:cambia a (b,c) xs (d+1,e)
	| d == b-1 = a:xs
	| d<0 || d>=c = (x:xs)


modificar :: String -> (Int, Int) -> [[String]] -> (Int, Int) -> [[String]]
modificar a (b,c) (x:xs) (d,e)
	| e /= c-1 = x:modificar a (b,c) xs (d,e+1)
	| e == c-1 = (cambia a (b,c) x (d,e)):xs
	| e<0 || e>=c = (x:xs)
	
modifi :: String -> [(Int, Int)] -> [[String]] -> (Int, Int) -> [[String]]
modifi _ [] xs _ = xs
modifi a (y:ys) (x:xs) (d,e) = modificar a y (modifi a ys (x:xs) (d,e)) (d,e)

editor :: [String] -> [[(Int, Int)]] -> [[String]] -> (Int, Int) -> [[String]]
editor _ [] xs _ = xs
editor [] _ xs _ = xs
editor [a1, a2] [y1,y2] xs (d,e) = modifi a1 y1 (modifi a2 y2 xs (d,e)) (d,e)

main = do
print $ cartesiano [1,2] [3,4] [5,6]
print $ ""
print $ ""
print $ ""
imprimir $ editor ["A", " "] [[(13,2),(12,3),(14,3),(11,4),(15,4),(10,5),(16,5),(9,6),(17,6),(18,7),(8,7),(19,8),(7,8),(20,9),(6,9),(19,10),(7,10),(18,11),(8,11), (17,12),(9,12),(16,13),(10,13),(15,14),(11,14),(14,15),(12,15),(13,16)], [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1),(11,1),(12,1)]] mapa (0,0)
