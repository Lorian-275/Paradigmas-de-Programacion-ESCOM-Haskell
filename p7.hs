import System.Random

frases = ["Es cierto","Es decididamente asi","Sin duda","Si definitivamente","Puedes confiar en ello","Como yo lo veo, si","Mis probable","Perspectivas buena","Las señales apuntan a que si","Si","Respuesta confusa, intenta otra vez","Pregunta de nuevo mas tarde","Mejor no decirte ahora","No se puede predecir ahora","Concentrate y pregunta otra vez"]

randomList :: StdGen ->Double ->Int -> [Int]
randomList g m n = map truncate (map (*m) (take n (randoms (g) :: [Double])))

randomInt :: StdGen -> Double -> Int
randomInt g x = head (randomList g x 1)

--bola8 :: Int -> [String] -> String
--bola8 x ys = head (drop (x-1) ys)

bola8 :: IO()
bola8 = do
    g <- newStdGen
    print $ head (drop (randomInt g 15) frases)

dados :: IO()
dados = do
    g1 <- newStdGen
    let d1 = randomInt g1 6
    print $ "Dado 1: "++ show d1
    g2 <- newStdGen
    let d2 = randomInt g2 6
    print $ "Dado 2: "++ show d2
    print $ "Suma: " ++ show (d1+d2)
    if (d1+d2)==7 then (print $ "Ganaste") else (print $ "Perdiste")

--dados :: Int -> Int -> String
--dados x y
--    | x+y == 7 = "Ganaste"
--    | otherwise = "Perdiste"
        
main = do
print $ "Funcion 2. Dados"
--g1 <- newStdGen
--g2 <- newStdGen
--print $ dados (randomInt g1 5) (randomInt g2 5)
dados

print $ "-------------------------------------------"
print $ "Funcion 4. Bola 8"
--g <- newStdGen
--let num = randomInt g 15
--print $ bola8 num frases
bola8

