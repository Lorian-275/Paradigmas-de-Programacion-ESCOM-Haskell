import Data.Char (isSpace)

import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Expr
import Text.Parsec.Language ( emptyDef )
import Control.Monad.RWS (Sum)
import GHC.IO.Buffer (Buffer(Buffer))

-- Definimos ciertos tipos de datos
data BinarioOP = Suma | Resta | Mult | Div | Pow deriving Eq

data UnarioOp = Menos | Sen | Cos | Tan | Sec | Log | Exp | Sqrt deriving Eq

data Expr = Var Char
            | Const Double
            | BinExp BinarioOP Expr Expr
            | UnExp UnarioOp Expr
            deriving Eq

--Aqui es donde se imprimira las expresiones como resultado
instance Show BinarioOP where
    show Suma = " + "
    show Resta = " - "
    show Mult = " * "
    show Div = " / "
    show Pow = " ^ "

instance Show UnarioOp where
    show Menos = "-"
    show Sen = "sen"
    show Cos = "cos"
    show Tan = "tan"
    show Sec = "sec"
    show Log = "log"
    show Exp = "exp"
    show Sqrt = "sqrt"

instance Show Expr where
    show (Var a) = "(" ++ show a ++ ")"
    show (Const a) = "(" ++ show a ++ ")"
    show (BinExp a b c) = "(" ++ show b ++ show a ++ show c ++ ")"
    show (UnExp a b) = "(" ++ show a ++ show b ++ ")"

--Funcion que simplifica expresiones
simplifica :: Expr -> Expr
    --Reglas de simplificacion de suma
simplifica (BinExp Suma (Const a) (Const b)) = Const (a + b)                                                    --a + b = a + b
simplifica (BinExp Suma a (Const 0)) = simplifica a                                                             --a + 0 = a
simplifica (BinExp Suma (Const 0) a) = simplifica a                                                             --0 + a = 0

simplifica (BinExp Suma (BinExp Mult (Const a) b) (BinExp Mult (Const c) d))
    |b == d = simplifica (BinExp Mult (Const (a + c)) b)                                                        --a*f(x) + c*f(x) = (a+c)*f(x)
    |otherwise = BinExp Suma (simplifica (BinExp Mult (Const a) b)) (simplifica (BinExp Mult (Const c) d))      --a*f(x) + c*g(x)
simplifica (BinExp Suma (BinExp Mult (Const a) b) (BinExp Mult d (Const c))) = simplifica (BinExp Suma (BinExp Mult (Const a) b) (BinExp Mult (Const c) d))     --a*f(x) + f(x)*d = (a+d)*f(x)
simplifica (BinExp Suma (BinExp Mult b (Const a)) (BinExp Mult (Const c) d)) = simplifica (BinExp Suma (BinExp Mult (Const a) b) (BinExp Mult (Const c) d))     --f(x)*b + c*f(x) = (b+c)*f(x)
simplifica (BinExp Suma (BinExp Mult b (Const a)) (BinExp Mult d (Const c))) = simplifica (BinExp Suma (BinExp Mult (Const a) b) (BinExp Mult (Const c) d))     --f(x)*b + f(x)*d = (b+d)*f(x)

simplifica (BinExp Suma a (BinExp Mult (Const b) c))
    |a == c = simplifica (BinExp Mult (Const (b+1)) a)                                                          --f(x) + b*f(x) = (b+1)*f(x)
    |otherwise = BinExp Suma (simplifica a) (simplifica (BinExp Mult (Const b) c))                              --f(x) + b*g(x)
simplifica (BinExp Suma a (BinExp Mult c (Const b))) = simplifica (BinExp Suma a (BinExp Mult (Const b) c))
simplifica (BinExp Suma (BinExp Mult (Const b) c) a) = simplifica (BinExp Suma a (BinExp Mult (Const b) c))
simplifica (BinExp Suma (BinExp Mult c (Const b)) a) = simplifica (BinExp Suma a (BinExp Mult (Const b) c))

simplifica (BinExp Suma a b)
    |a == b = BinExp Mult (Const 2) (simplifica a)                                                              --f(x) + f(x) = 2*f(x)
    |otherwise = BinExp Suma (simplifica a) (simplifica b)                                                      --f(x) + g(x)

    --Reglas de simplificacion de resta
simplifica (BinExp Resta (Const a) (Const b)) = Const (a - b)                                                   --a - b = a - b
simplifica (BinExp Resta a (Const 0)) = simplifica a                                                            --a + 0 = a
simplifica (BinExp Resta (Const 0) a) = simplifica (UnExp Menos a)                                              --0 + a = a

simplifica (BinExp Resta (BinExp Mult (Const a) b) (BinExp Mult (Const c) d))
    |b == d = simplifica (BinExp Mult (Const (a - c)) b)                                                        --a*f(x) - c*f(x) = (a-c)*f(x)
    |otherwise = BinExp Resta (simplifica (BinExp Mult (Const a) b)) (simplifica (BinExp Mult (Const c) d))     --a*f(x) - c*g(x)
simplifica (BinExp Resta (BinExp Mult (Const a) b) (BinExp Mult d (Const c))) = simplifica (BinExp Resta (BinExp Mult (Const a) b) (BinExp Mult (Const c) d))   --a*f(x) - f(x)*d = (a-d)*f(x)
simplifica (BinExp Resta (BinExp Mult b (Const a)) (BinExp Mult (Const c) d)) = simplifica (BinExp Resta (BinExp Mult (Const a) b) (BinExp Mult (Const c) d))   --f(x)*b - c*f(x) = (b-c)*f(x)
simplifica (BinExp Resta (BinExp Mult b (Const a)) (BinExp Mult d (Const c))) = simplifica (BinExp Resta (BinExp Mult (Const a) b) (BinExp Mult (Const c) d))   --f(x)*b - f(x)*d = (b-d)*f(x)

simplifica (BinExp Resta a (BinExp Mult (Const b) c))
    |a == c = simplifica (BinExp Mult (Const (1 - b)) a)                                                        --f(x) - b*f(x) = (1-b)*f(x)
    |otherwise = BinExp Resta (simplifica a) (simplifica (BinExp Mult (Const b) c))                             --f(x) - b*g(x)
simplifica (BinExp Resta a (BinExp Mult c (Const b))) = simplifica (BinExp Resta a (BinExp Mult (Const b) c))   --f(x) - f(x)*b

simplifica (BinExp Resta (BinExp Mult (Const b) c) a)
    |c == a = simplifica (BinExp Mult (Const (b - 1)) a)                                                        --b*f(x) - f(x) = (b-1)*f(x)
    |otherwise = BinExp Resta (simplifica (BinExp Mult (Const b) c)) (simplifica a)                             --b*f(x) - g(x)
simplifica (BinExp Resta (BinExp Mult c (Const b)) a) = simplifica (BinExp Resta (BinExp Mult (Const b) c) a)

simplifica (BinExp Resta a b)
    |a == b = Const 0                                                                                           --f(x) - f(x) = 0
    |otherwise = BinExp Resta (simplifica a) (simplifica b)                                                     --f(x) - g(x)

    --Reglas de simplificacion de multiplicacion
simplifica (BinExp Mult (Const a) (Const b)) = Const (a * b)                                                    --a * b = ab
simplifica (BinExp Mult (Const 0) a) = Const 0                                                                  --0 * f(x) = 0
simplifica (BinExp Mult a (Const 0)) = Const 0                                                                  --f(x) * 0 = 0
simplifica (BinExp Mult (Const 1) a) = simplifica a                                                             --1 * f(x) = f(x)
simplifica (BinExp Mult a (Const 1)) = simplifica a                                                             --f(x) * 1 = f(x)
simplifica (BinExp Mult (Const (-1)) a) = UnExp Menos (simplifica a)                                            --(-1) * f(x) = -f(x)
simplifica (BinExp Mult a (Const (-1))) = UnExp Menos (simplifica a)                                            --f(x) * (-1) = -f(x)

simplifica (BinExp Mult (BinExp Div (Const 1) a) b) = simplifica (BinExp Div b a)                               --(1/g(x)) * f(x) = f(x) / g(x)
simplifica (BinExp Mult b (BinExp Div (Const 1) a)) = simplifica (BinExp Div b a)                               --f(x) * (1/g(x)) = f(x) / g(x)

simplifica (BinExp Mult (Const a) (BinExp Mult (Const b) c)) = BinExp Mult (Const (a*b)) (simplifica c)
simplifica (BinExp Mult (Const a) (BinExp Mult c (Const b))) = BinExp Mult (Const (a*b)) (simplifica c)
simplifica (BinExp Mult (BinExp Mult (Const b) c) (Const a)) = BinExp Mult (Const (a*b)) (simplifica c)
simplifica (BinExp Mult (BinExp Mult c (Const b)) (Const a)) = BinExp Mult (Const (a*b)) (simplifica c)

simplifica (BinExp Mult a (BinExp Mult (Const b) c)) = BinExp Mult (Const b) (simplifica (BinExp Mult a c))
simplifica (BinExp Mult a (BinExp Mult c (Const b))) = BinExp Mult (Const b) (simplifica (BinExp Mult a c))
simplifica (BinExp Mult (BinExp Mult (Const b) c) a) = BinExp Mult (Const b) (simplifica (BinExp Mult a c))
simplifica (BinExp Mult (BinExp Mult c (Const b)) a) = BinExp Mult (Const b) (simplifica (BinExp Mult a c))

simplifica (BinExp Mult (BinExp Pow a (Const b)) (BinExp Pow c (Const d)))
    |a == c = simplifica (BinExp Pow a (Const (b+d)))                                                           --f(x)^b * f(x)^d = f(x)^(b+d)
    |otherwise = BinExp Mult (simplifica (BinExp Pow a (Const b))) (simplifica (BinExp Pow c (Const d)))        --f(x)^b * g(x)^d

simplifica (BinExp Mult a (BinExp Pow b (Const c)))                                                             
    |a == b = BinExp Pow (simplifica a) (Const (c+1))                                                           --f(x) * f(x)^c = f(x)^(c+1)
    |otherwise = BinExp Mult (simplifica a) (simplifica (BinExp Pow b (Const c)))                               --f(x) * g(x)^c
simplifica (BinExp Mult (BinExp Pow b (Const c)) a) = simplifica (BinExp Mult a (BinExp Pow b (Const c)))       --f(x)^c * f(x) = f(x)^(c+1)

simplifica (BinExp Mult a b)
    |a == b = BinExp Pow (simplifica a) (Const 2)                                                               --f(x) * f(x) = f(x)^2
    |otherwise = BinExp Mult (simplifica a) (simplifica b)                                                      --f(x) * g(x)

    --Reglas de simplificacion de division
simplifica (BinExp Div (Const 0) a) = Const 0                                                                   --0 / f(x) = 0
simplifica (BinExp Div a (Const 0)) = error "Division por cero"                                                 --f(x) / 0 = error
simplifica (BinExp Div (Const a) (Const b)) = Const (a / b)                                                     --a / b
simplifica (BinExp Div a (Const 1)) = simplifica a                                                              --a / 1 = a
simplifica (BinExp Div a (Const (-1))) = simplifica (UnExp Menos a)                                             --a / (-1) = -a

simplifica (BinExp Div (UnExp Menos (UnExp Sen a)) (UnExp Cos b)) 
    |a == b = UnExp Menos (UnExp Tan a)
    |otherwise = BinExp Div (simplifica (UnExp Sen a)) (simplifica (UnExp Cos b))
 
simplifica (BinExp Div (Const a) (BinExp Mult (Const b) c)) = BinExp Div (Const (a/b)) (simplifica c)
simplifica (BinExp Div (Const a) (BinExp Mult c (Const b))) = BinExp Div (Const (a/b)) (simplifica c)
simplifica (BinExp Div (BinExp Mult (Const b) c) (Const a)) = BinExp Mult (Const (b/a)) (simplifica c)
simplifica (BinExp Div (BinExp Mult c (Const b)) (Const a)) = BinExp Mult (Const (b/a)) (simplifica c)

simplifica (BinExp Div a (BinExp Mult (Const b) c)) = BinExp Mult (Const (1/b)) (simplifica (BinExp Div a c))
simplifica (BinExp Div a (BinExp Mult c (Const b))) = BinExp Mult (Const (1/b)) (simplifica (BinExp Div a c))
simplifica (BinExp Div (BinExp Mult (Const b) c) a) = BinExp Mult (Const b) (simplifica (BinExp Div c a))
simplifica (BinExp Div (BinExp Mult c (Const b)) a) = BinExp Mult (Const b) (simplifica (BinExp Div c a))

simplifica (BinExp Div (BinExp Pow a (Const b)) (BinExp Pow c (Const d)))
    |a == c = simplifica (BinExp Pow a (Const (b-d)))                                                           --f(x)^b / f(x)^d = f(x)^(b-d)
    |otherwise = BinExp Div (simplifica (BinExp Pow a (Const b))) (simplifica (BinExp Pow c (Const d)))         --f(x)^b / g(x)^d

simplifica (BinExp Div a (BinExp Pow b (Const c)))
    |a == b = simplifica (BinExp Pow a (Const (1-c)))                                                           --f(x) / f(x)^c = f(x)^(1-c)
    |otherwise = BinExp Div (simplifica a) (simplifica (BinExp Pow b (Const c)))                                --f(x) / g(x)^c

simplifica (BinExp Div (BinExp Pow b (Const c)) a)
    |b == a = simplifica (BinExp Pow a (Const (c-1)))                                                           --f(x)^c / f(x) = f(x)^(c-1)
    |otherwise = BinExp Div (simplifica (BinExp Pow b (Const c))) (simplifica a)                                --g(x)^c / f(x)

simplifica (BinExp Div a b)
    |a == b = Const 1                                                                                           --f(x) / f(x) = 1
    |otherwise = BinExp Div (simplifica a) (simplifica b)                                                       --f(x) / g(x)

    --Reglas de simplificacion de potencia
simplifica (BinExp Pow (Const a) (Const b)) = Const (a ** b)
simplifica (BinExp Pow a (Const 0)) = Const 1
simplifica (BinExp Pow a (Const 1)) = simplifica a
simplifica (BinExp Pow (BinExp Pow a (Const b)) (Const c)) = BinExp Pow (simplifica a) (Const (b * c))
simplifica (BinExp Pow a b) = BinExp Pow (simplifica a) (simplifica b)

    --Reglas de simplificacion de la negacion
simplifica (UnExp Menos (Const a)) = Const (-a)
simplifica (UnExp Menos (UnExp Menos a)) = simplifica a
simplifica (UnExp Menos (BinExp Suma a b)) = BinExp Suma (UnExp Menos (simplifica a)) (UnExp Menos (simplifica b))
simplifica (UnExp Menos (BinExp Resta a b)) = BinExp Resta (simplifica b) (simplifica a)
simplifica (UnExp Menos (BinExp Mult a b)) = BinExp Mult (UnExp Menos (simplifica a)) (simplifica b)
simplifica (UnExp Menos (BinExp Div a b)) = BinExp Div (UnExp Menos (simplifica a)) (simplifica b)
simplifica (UnExp Menos a) = UnExp Menos (simplifica a)

    --Regla de simplificacion por default
simplifica x = x

--Funcion para simplificar repetidamente hasta que la expresion permanece sin cambios
simplificaTodo :: Expr -> Expr
simplificaTodo a = simplificaTodo' a (Const 0) where
    simplificaTodo' curr last | curr == last = curr
                              | otherwise = simplificaTodo' curr' curr where
                                  curr' = simplifica curr

derivTodo :: Expr -> Expr
derivTodo a = simplificaTodo $ deriv $ simplificaTodo a

--Derivadas
deriv :: Expr -> Expr
deriv (Const a) = Const 0
deriv (Var a) = Const 1
deriv (BinExp Suma a b) = BinExp Suma (deriv a) (deriv b)
deriv (BinExp Resta a b) = BinExp Resta (deriv a) (deriv b)
deriv (BinExp Mult (Const a) b) = BinExp Mult (Const a) (deriv b)
deriv (BinExp Mult a (Const b)) = BinExp Mult (Const b) (deriv a)
deriv (BinExp Mult a b) = BinExp Suma (BinExp Mult a (deriv b)) (BinExp Mult (deriv a) b)
deriv (BinExp Div a b) = BinExp Div (BinExp Resta (BinExp Mult (deriv a) b) (BinExp Mult a (deriv b))) (BinExp Pow b (Const 2))
deriv (BinExp Pow a (Const b)) = BinExp Mult (Const b) (BinExp Pow a (Const (b - 1)))
deriv (BinExp Pow a b) = error "diferenciar potencias, no sera admitida sin parentesis"
deriv (UnExp Menos a) = UnExp Menos (deriv a)
deriv (UnExp Sen a) = BinExp Mult (UnExp Cos a) (deriv a)
deriv (UnExp Cos a) = BinExp Mult (UnExp Menos (UnExp Sen a)) (deriv a)
deriv (UnExp Tan a) = BinExp Mult (BinExp Pow (UnExp Sec a) (Const 2)) (deriv a)
deriv (UnExp Sec a) = BinExp Mult (BinExp Mult (UnExp Sec a) (UnExp Tan a)) (deriv a)
deriv (UnExp Log a) = BinExp Mult (BinExp Div (Const 1) a) (deriv a)
deriv (UnExp Exp a) = BinExp Mult (UnExp Exp a) (deriv a)
deriv (UnExp Sqrt a) = BinExp Mult (BinExp Div (Const 0.5) (UnExp Sqrt a)) (deriv a)

--Generar Lexer
def = emptyDef {
    T.opLetter = oneOf "+-*/^~nxcgpt",
    T.reservedOpNames = ["+", "-", "*", "/", "^", "~", "sin", "cos", "tan", "sec", "log", "exp", "sqrt"]
}

lexer = T.makeTokenParser def

op = T.reservedOp lexer
float = T.float lexer
parens = T.parens lexer

--Contruye el analizador
exprParser = buildExpressionParser opTable expr

opTable = [ [Infix (op "^"      >> return (BinExp Pow)) AssocRight  ],
            [Infix (op "/"      >> return (BinExp Div)) AssocLeft   ],
            [Infix (op "*"      >> return (BinExp Mult)) AssocLeft  ],
            [Infix (op "-"      >> return (BinExp Resta)) AssocLeft ],
            [Infix (op "+"      >> return (BinExp Suma)) AssocLeft  ],
            [Prefix (op "~"     >> return (UnExp Menos))            ],
            [Prefix (op "sin"   >> return (UnExp Sen))              ],
            [Prefix (op "cos"   >> return (UnExp Cos))              ],
            [Prefix (op "tan"   >> return (UnExp Tan))              ],
            [Prefix (op "sec"   >> return (UnExp Sec))              ],
            [Prefix (op "log"   >> return (UnExp Log))              ],
            [Prefix (op "exp"   >> return (UnExp Exp))              ],
            [Prefix (op "sqrt"  >> return (UnExp Sqrt))              ]]

expr = fmap Var (char 'x')
    <|> fmap Const float
    <|> parens exprParser