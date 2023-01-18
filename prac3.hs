--contar cuantos elementos de tipo x hay en la lista
contar :: (Eq a) => [a] -> a-> Int
contar ys y = foldl(\acc x -> if x==y then acc+1 else acc) 0 ys

--sumar los cubos de una lista
suma::Int -> Int
suma n = foldl(\acu x -> (x*x*x) + acu) 0 [1..n]


main = do 
    print(contar "rabiosos osos polacos" 'o')
    print(suma 6)