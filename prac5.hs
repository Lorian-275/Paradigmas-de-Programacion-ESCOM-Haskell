import Data.Array

noi :: (Eq a1, Num a1) => (a1, a2, a2, a2) -> [(a2, a2)]
noi(0, _, _, _) = []
noi(n, ori, dest, aux) = noi(n-1, ori, aux, dest) ++ [(ori, dest)] ++ noi(n-1, aux, dest, ori)
hanoi x = noi(x, 'a', 'c', 'b')

--tablas multiplicar
g :: Int -> [Int] -> [Int]
g _ [] = []
g a (x:xs) = a*x : g a xs

tablmult :: [Int] -> [Int] -> [[Int]]
tablmult [] _ = [[]]
tablmult (x:xs) b = g x b : tablmult xs b

--matriz suma
suma :: [[Int]] -> [[Int]] -> [[Int]]
suma [[x1,x2],[y1,y2]] [[a1,a2],[b1,b2]]  = [[x1 + a1,x2+a2],[y1+b1,y2+b2]]

main = do
    print (hanoi 3)
    print (tablmult [1,2,3,4,5,6,7,8,9,10] [1,2,3,4,5,6,7,8,9,10])
    print(suma [[1,2],[3,4]] [[2,4],[6,8]])
