{-# LANGUAGE GADTs, KindSignatures, TypeOperators #-}

--data a :*: b = Foo a b
data List :: * -> * where
    (:|) :: a -> List a -> List a
    Nil    :: List a
infixr 5 :|

instance (Read a) => Read (List a) where
    readsPrec i (' ':cs) = readsPrec i cs
    readsPrec _ ('N':'i':'l':cs) = [(Nil, cs)]
    readsPrec i string = let (as,bs) = f string
                             ((x,rest):_) = readsPrec i bs
                             in [((read as :| x), rest)]
                              where f []        = ("","")
                                    f (' ':cs)  = f cs
                                    f (':':'|': cs) = ("", cs)
                                    f ('\\':c:cs) = let (y, ys) = f cs
                                                     in ('\\':c:y, ys)
                                    f ('\'':cs)   = let (y, ys) = g '\'' cs
                                                     in ('\'':y, ys)
                                    f ('"':cs)    = let (y, ys) = g '"' cs
                                                     in ('"':y, ys)
                                    f (c:cs)       = let (y, ys) = f cs
                                                     in (c:y, ys)
                                    g d ('\\':c:cs) = let (y, ys) = g c cs
                                                                  in ('\\':c:y, ys)
                                    g d (c:cs) | d == c    = let (y, ys) = f cs
                                                                in (c:y, ys)
                                               | otherwise = let (y, ys) = g d cs
                                                                  in (c:y, ys)
                                    g d [] = error [d]
                                                     
instance (Show a) => Show (List a) where
    showsPrec i Nil s = "Nil" ++ s
    showsPrec i (a :| b ) s = show a ++ ':':'|':(showsPrec i b s)
    
(++|) :: List a -> List a -> List a
(++|) Nil l      = l
(++|) (x:|Nil) l = (x :| l )
(++|) (x:|xs) l  = x :| (xs ++| l)

h :: List a -> a
h (x:|_) = x
h Nil    = error "h of empty list"
t :: List a -> List a
t (_:|xs) = xs
t Nil     = error "t of empty list"
i :: List a -> List a
i (x:|xs) = i' x xs where i' x Nil = Nil; i' x (y:|ys) = x:|(i' y ys) 
i Nil     = error "i of empty list"
u :: List a -> Maybe (a,List a)
u (x:|xs) = Just (x,xs)
u Nil     = Nothing

infixr 5 ++|
