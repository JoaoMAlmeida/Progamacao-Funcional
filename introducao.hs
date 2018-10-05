{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor a b = (a || b) && not(a && b)
impl a b = (not a || b)
equiv a b = (impl a b) && (impl b a)

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y
-}
pow x y = product (take y (repeat x))



{-
- Implemente a funcao fatorial que calcula o fatorial de um numero
-}
fatorial x = product [1..x]

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime x = not (elem 0 (map (mod x) [2..x-1]))

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas.
-}
fib x =
  if (x == 0 || x == 1) then x
  else fib (x - 1) + fib (x - 2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides.
-}
mdc x y
  | x < y = mdc y x
  | y == 0 = x
  | otherwise = mdc y (mod x y)

{-
- Calcula um MMC de dois numeros.
-}
mmc x y = head (filter (divisivel x y) [(min x y)..x*y])

-- Auxiliar
divisivel x y n = (mod n x == 0) && (mod n y == 0)

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True
-}
coprimo x y = (mdc x y == 1)

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = head [(a,b) | a <- filter isPrime [1..x-1], b <- filter isPrime [1..x-1], a + b == x]
