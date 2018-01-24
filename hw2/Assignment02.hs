module Assignment02 where

data Numb = Z | S Numb deriving Show

add :: Numb -> Numb -> Numb
add m n = case n of {Z -> m; S n' -> S (add m n')}

one, two, three, four, five, six :: Numb
one = S Z
two = S one
three = S two
four = S three
five = S four
six = S five

data NumbList = EmptyNL | NonEmptyNL Numb NumbList deriving Show

list0, list1, list2 :: NumbList
list0 = NonEmptyNL one (NonEmptyNL two (NonEmptyNL three EmptyNL))
list1 = NonEmptyNL four (NonEmptyNL Z (NonEmptyNL two EmptyNL))
list2 = NonEmptyNL six (NonEmptyNL one (NonEmptyNL three (NonEmptyNL four EmptyNL)))

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

sumUpTo :: Numb -> Numb
sumUpTo n =
	case n of
	Z -> Z
	S n' -> add (S n') (sumUpTo n')


equal :: Numb -> Numb -> Bool
equal x y = case x of
  Z -> case y of
       Z -> True
       S y' -> False
  S x' -> case y of 
          Z -> False
          S y' -> (equal x' y')

difference :: Numb -> Numb -> Numb
difference x y = case x of
  Z -> case y of
       Z -> Z
       S y' -> y
  S x' -> case y of 
          Z -> x
          S y' -> (difference x' y')

total :: NumbList -> Numb
total l = case l of 
  EmptyNL -> Z
  NonEmptyNL l l' -> add l (total l')

incrementAll :: Numb -> NumbList -> NumbList
incrementAll n l = case l of 
  EmptyNL -> EmptyNL
  NonEmptyNL l l' -> NonEmptyNL (add n l) (incrementAll n l')

addToEnd :: Numb -> (NumbList -> NumbList)
addToEnd n nl = case nl of
	 EmptyNL -> NonEmptyNL n nl
	 NonEmptyNL x nl' -> NonEmptyNL x (addToEnd n nl')
	 
lastElement :: NumbList -> Numb
lastElement l = case l of
  EmptyNL -> Z
  NonEmptyNL l l' -> case l' of
                     EmptyNL -> l
                     _ -> lastElement l'

contains :: (Numb -> Bool) -> NumbList -> Bool
contains f l = case l of
  EmptyNL -> False
  NonEmptyNL l l' -> if f l then True else contains f l'

remove :: (Numb -> Bool) -> NumbList -> NumbList
remove f l = case l of
  EmptyNL -> EmptyNL
  NonEmptyNL l l' -> if f l then remove f l' else NonEmptyNL l (remove f l')

append :: NumbList -> NumbList -> NumbList
append l1 l2 = case l1 of
  EmptyNL -> l2
  NonEmptyNL l1 l1' -> NonEmptyNL l1 (append l1' l2)

prefix :: Numb -> NumbList -> NumbList
prefix n l = case n of
  Z -> EmptyNL
  S n' -> case l of 
          EmptyNL -> EmptyNL
          NonEmptyNL l l' -> NonEmptyNL l (prefix n' l')