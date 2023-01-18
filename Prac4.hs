
data Racional = Racional{
    num :: Int,
    denominador :: Int
}deriving (Show, Eq)

-- Multiplicacion de dos numeros racionales -- 
multiplicacion :: Racional -> Racional -> Racional
multiplicacion (Racional a v) (Racional x y) = Racional (a*x) (v*y)

-- Suma de dos numeros racionales --
suma :: Racional -> Racional -> Racional
suma (Racional a v) (Racional x y) = Racional ((a*y)+(v*x)) (v*y)

-- Resta de dos numeros racionales -- 
resta :: Racional -> Racional -> Racional
resta (Racional a v) (Racional x y) = Racional ((a*y)-(v*x)) (v*y)

-- Comprueba si dos numeros racionales son iguales o no --
igual :: Racional -> Racional -> Bool
igual (Racional a v) (Racional x y)
        | (a*y) == (v*x) = True
        | otherwise = False


main = do
    let rac1 = Racional{num = 1, denominador = 5}
    let rac2 = Racional{num = 1, denominador = 5}
    putStrLn "\nLa mutiplicacion de dichos racionales es: "
    print (multiplicacion rac1 rac2)
    putStrLn "\nLa suma de dichos racionales es: "
    print (suma rac1 rac2)
    putStrLn "\nLa resta de dichos racionales es: "
    print (resta rac1 rac2)
    putStrLn "\nLa igualdad de dichos racionales es: "
    print (igual rac1 rac2)
    
    