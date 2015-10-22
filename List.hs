{-# LANGUAGE GADTs, KindSignatures, TypeOperators #-}

module List (List(..),h,i,l,t,u,m,r,inters,(++|)) where

import Prelude hiding (foldr)
import Data.Foldable 
import Control.Applicative

import Data.Foldable 
data List :: * -> * where
    (:|) :: a -> List a -> List a
    Nil    :: List a
infixr 5 :|

instance Eq a => Eq (List a) where
    (x:|xs) == (y:|ys) = x == y && xs == ys
    Nil == Nil         = True
    _ == _             = False
    
instance Functor List where
    fmap = m

instance Ord a => Ord (List a) where
    compare Nil Nil = EQ
    compare _ Nil   = GT
    compare Nil _   = LT
    compare (x:|xs) (y:|ys) | x == y    = compare xs ys
                            | otherwise = compare x y

instance Foldable List where
    foldr f b Nil     = b
    foldr f b (x:|xs) = f x (foldr f b xs)
    
instance Applicative List where
    pure a = a:|Nil
    Nil <*> _ = Nil
    (x:|xs) <*> l = m x l ++| (xs <*> l)
    
                            
instance Monad List where
    l >>=f = foldr (++|) Nil (m f l)
    return a = a:|Nil

instance (Read a) => Read (List a) where
    readsPrec i (' ':cs)             = readsPrec i cs
    readsPrec _ ('N':'i':'l':cs)     = [(Nil, cs)]
    readsPrec _ (':':'[':']':':':cs) = [(Nil, cs)]
    readsPrec i (':':'[':string)     = let (as,bs) = readTo2 ']' ':' string
                                           readList "" = Nil
                                           readList str = (read c):|(readList cs) where (c, cs) = readTo ',' str
                                        in [((readList as), bs)]
    readsPrec i string               = let (as,bs) = readTo2 ':' '|' string
                                           ((x,rest):_) = readsPrec i bs
                                        in [((read as :| x), rest)]

readTo :: Char -> String -> (String,String)
readTo d [] = ("","")
readTo d ('\\':c:cs) = let (y, ys) = readTo d cs in ('\\':c:y, ys)
readTo d (c:cs) | d == c    = ("",cs)
                | otherwise = let (y, ys) = readTo d cs in (c:y, ys)

readTo2 :: Char -> Char -> String -> (String,String)
readTo2 d e [] = ("","")
readTo2 d e ('\\':c:cs) = let (y, ys) = readTo2 d e cs in ('\\':c:y, ys)
readTo2 d e ('\'':cs)   = let (y, ys) = (readTo '\'' cs)
                              (z, zs) = (readTo2 d e ys) 
                           in (y++'\'':z, zs)
readTo2 d e ('"':cs)    = let (y, ys) = (readTo '"' cs)
                              (z, zs) = (readTo2 d e ys) 
                           in (y++'"':z, zs)
readTo2 d e (c1:c2:cs) | d == c1 && e == c2 = ("", cs)
                       | otherwise          = let (y, ys) = (readTo2 d e (c2:cs)) in (c1:y, ys)
 

instance (Show a) => Show (List a) where
    showsPrec i Nil s    = "Nil" ++ s
    showsPrec i (x:|xs) s = let showList Nil = ']':':':s
                                showList (a:|as) = ',':(show a ++ (showList as))
                             in ':':'[':((show x) ++ (showList xs))

(++|) :: List a -> List a -> List a
(++|) Nil l      = l
(++|) (x:|Nil) l = (x :| l )
(++|) (x:|xs) l  = x :| (xs ++| l)

h :: List a -> a
h (x:|_) = x
h Nil    = error "h of empty list"
l :: List a -> a
l (x:|Nil) = x
l (x:|xs)  = l xs
l Nil      = error "l of empty list"
t :: List a -> List a
t (_:|xs) = xs
t Nil     = error "t of empty list"
i :: List a -> List a
i (x:|xs) = i' x xs where i' x Nil = Nil; i' x (y:|ys) = x:|(i' y ys) 
i Nil     = error "i of empty list"
u :: List a -> Maybe (a,List a)
u (x:|xs) = Just (x,xs)
u Nil     = Nothing

m :: (a -> b) -> List a -> List b
m f Nil    = Nil
m f (x:|xs) = (f x) :| (m f xs)

r :: List a -> List a
r = let r' l Nil  = l
        r' l (x:|xs) = r' (x:|l) xs
     in r' Nil 

inters :: a -> List a -> List a
inters _ Nil      = Nil
inters _ (x:|Nil) = (x:|Nil)
inters y (x:|xs)  = (x:|y:|(inters y xs))



infixr 5 ++|
