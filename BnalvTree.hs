bnalvLTree :: LTree a -> (BTree Bool -> LTree a)
bnalvLTree arv Empty = arv 
bnalvLTree (Leaf p) _ = Leaf p
bnalvLTree (Fork (a,b)) (Node (c,(l,r))) = if c == True then bnalvLTree a l else bnalvLTree b r


bnalvLTree :: LTree a -> (BTree Bool -> LTree a)
bnalvLTree = cataLTree (either g1 g2) where
            g1 p _ =  Leaf p
            g2 (a,b) Empty = Fork (a Empty, b Empty)
            g2 (a,b) (Node (c,(l,r))) = if c == True then a l else b r



pbnalvLTree :: LTree a -> ((BTree (Dist Bool)) -> Dist (LTree a))
pbnalvLTree = cataLTree (either g1 g2) where
            g1 p _ =  D [(Leaf p, 1)]
            g2 (a,b) Empty =  aux (prod (a Empty) (b Empty))
            g2 (a,b) (Node (c,(l,r))) = do {final <- c ;if final then a l else b r}


aux :: Dist (a,b) -> LTree a
aux (a,b) =  do {f <- a; h <- b; return Fork(a,b)}

anita = Query ("2a-feira?", (Query ("Chuva na ida", (Dec "Precisa", Query ("Chuva no regresso", (Dec "Precisa", Dec "N Precisa")))), Dec "N Precisa"))

pbnavLTree (extLTree anita) (Node ((D [(True, 0.8)]), (Node ((D [(True, 0.6)]), (Empty, Empty)), Empty)))

pbnavLTree (extLTree anita) (Node (D [(True, 0.143)], ((Node ((D [(True, 0.8)]), (Node ((D [(True, 0.6)]), (Empty, Empty)), Empty))) , Empty)))

