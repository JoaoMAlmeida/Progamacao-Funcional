module MultisetList ()
 where

{-
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde
 - cada elemento da lista consiste do dado em si e sua quantidade (um par).
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
-- import Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert elem [] = [(elem, 1)]
insert elem bag
  | elem == fst (head bag) = [(elem, snd (head bag) +1)] ++ (tail bag)
  | otherwise = [(head bag)] ++ (insert elem (tail bag))


{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura.
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove elem bag
  | elem == fst (head bag) =
    if ((snd (head bag)) > 1)
      then [(elem, snd (head bag) -1)] ++ (tail bag)
    else (tail bag)
  | otherwise = [(head bag)] ++ (remove elem (tail bag))


{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search elem [] =  0
search elem bag
  | elem == fst (head bag) = snd (head bag)
  | otherwise = (search elem (tail bag))

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union bag1 [] = bag1
union [] bag2 = bag2
union bag1 bag2 = [(first1, (max s1 s2))] ++ (union tail1 resto2)
  where
    h = head bag1
    first1 = fst h
    s1 = snd h
    s2 = search first1 bag2
    tail1 = tail bag1
    resto2 = [b | b <- bag2, (fst b) /= first1]


{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection bag1 [] = []
intersection [] bag2 = []
intersection bag1 bag2 = if (s2 > 0) then [(first1, (min s1 s2))] ++ (intersection tail1 resto2) else (intersection tail1 resto2)
  where
    h = head bag1
    first1 = fst h
    s1 = snd h
    s2 = search first1 bag2
    tail1 = tail bag1
    resto2 = [b | b <- bag2, (fst b) /= first1]

{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B).
     Caso essa quantidade seja negativa o elemento deve ser removido do Bag.
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus bag1 [] = bag1
minus [] _ = []
minus bag1 bag2
  | s2 == 0 = [h] ++ (minus tail1 resto2)
  | s2 >= s1 = (minus tail1 resto2)
  | otherwise = [(first1, (s1 - s2))] ++ (minus tail1 resto2)
  where
    h = head bag1
    first1 = fst h
    s1 = snd h
    s2 = search first1 bag2
    tail1 = tail bag1
    resto2 = [b | b <- bag2, (fst b) /= first1]

{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion [] _ = True
inclusion _ [] = False
inclusion bag1 bag2 = (s1 <= s2) && (inclusion tail1 resto2)
  where
    h = head bag1
    first1 = fst h
    s1 = snd h
    s2 = search first1 bag2
    tail1 = tail bag1
    resto2 = [b | b <- bag2, (fst b) /= first1]

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas.
-}
sum2 bag1 [] = bag1
sum2 [] bag2 = bag2
sum2 bag1 bag2 = [(first1, (s1 + s2))] ++ (sum2 tail1 resto2)
  where
    h = head bag1
    first1 = fst h
    s1 = snd h
    s2 = search first1 bag2
    tail1 = tail bag1
    resto2 = [b | b <- bag2, (fst b) /= first1]

{-
 - Retorna a quantidade total de elementos no Bag
-}
size [] = 0
size bag = 1 + (size (tail bag))
