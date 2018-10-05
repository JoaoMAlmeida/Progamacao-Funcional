--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
pow = \x y -> product (take y (repeat x))
fatorial = \x -> product [1..x]
isPrime = \x -> not (elem 0 (map (mod x) [2..x-1]))
fib = \x ->
  if (x == 0 || x == 1) then x
  else fib (x - 1) + fib (x - 2)
mdc = \x y ->
  if (x < y) then mdc y x
  else if (y == 0) then x
  else mdc y (mod x y)
divisivel = \x y n -> (mod n x == 0) && (mod n y == 0) -- Auxiliar para o mmc
mmc = \x y -> head (filter (divisivel x y) [(min x y)..x*y])
coprimo = \x y -> (mdc x y == 1)
goldbach = \x -> head [(a,b) | a <- filter isPrime [1..x-1], b <- filter isPrime [1..x-1], a + b == x]

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
meuLast = \xs ->
  if ((length xs) == 0) then error "Lista vazia!"
  else if ((length xs) == 1) then (head xs)
  else meuLast (tail xs)
penultimo = \xs ->
  if ((length xs) < 2) then error "Lista sem penultimo"
  else (last (init xs))
elementAt = \i xs ->
  if ((length xs) == 0) then error "Lista Vazia!"
  else if (i == 1) then (head xs)
  else (elementAt (i - 1) (tail xs))
meuLength :: (Eq a, Num x) => [a] -> x
meuLength = \xs ->
  if (xs == []) then 0
  else (1 + (meuLength (tail xs)))
meuReverso = \xs ->
  if ((length xs) == 0) then []
  else ((last xs) : (meuReverso (init xs)))
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = \xs -> (xs == (meuReverso xs))
compress :: Eq a => [a] -> [a]
compress = \xs ->
  if ((length xs) == 0) then []
  else [(head xs)] ++ (compress [y | y <- xs , y /= (head xs)])
compact :: Eq a => [a] -> [a]
compact = \xs ->
  if ((length xs) == 0) then []
  else [(head xs)] ++ [y | y <- (tail xs), y == (head xs)] ++ (compact [z | z <- (tail xs), z /= (head xs)])
encode :: Eq a => [a] -> [(a,Int)]
encode = \xs ->
  if ((length xs) == 0) then []
  else [((head xs), (meuLength (filter ((head xs)==) ((head xs):(tail xs)))))] ++ (encode (filter ((head xs)/=) (tail xs)))
split = \xs i -> [(take i xs)] ++ [(drop i xs)]
slice = \xs imin imax -> (drop (imin - 1) (take imax xs))
insertAt = \el pos xs -> (take (pos - 1) xs) ++ [el] ++ (drop (pos - 1) xs)
minList (x:xs) = if (x < (minList xs)) then x else minList xs
minList = \xs ->
  if ((length xs) == 1) then xs
  else if ((head xs) < (minList xs))
sort = \xs ->
  if ((length xs) == 0) then []
  else ((minList xs) : (sort (remove x xs)))
mySum xs = undefined
maxList xs = undefined
buildPalindrome xs = undefined
mean xs = undefined
myAppend xs ys = undefined
