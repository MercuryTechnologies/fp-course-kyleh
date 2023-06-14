{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.State where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional
import qualified Data.Set as S
import qualified Prelude as P

{- $setup
 >>> import Test.QuickCheck.Function
 >>> import Data.List(nub)
 >>> import Test.QuickCheck
 >>> import qualified Prelude as P(fmap)
 >>> import Course.Core
 >>> import Course.List
 >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary
-}

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a
    = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) =
    f

{- | Run the `State` seeded with `s` and retrieve the resulting state.

 prop> \(Fun _ f) s -> exec (State f) s == snd (runState (State f) s)
-}
exec :: State s a -> s -> s
exec pipe = (snd . (runState pipe) $)

{- | Run the `State` seeded with `s` and retrieve the resulting value.

 prop> \(Fun _ f) s -> eval (State f) s == fst (runState (State f) s)
-}
eval :: State s a -> s -> a
eval pipe = (fst . (runState pipe) $)

{- | A `State` where the state also distributes into the produced value.

 >>> runState get 0
 (0,0)
-}
get :: State s s
get = State (\x -> (x, x))

{- | A `State` where the resulting state is seeded with the given value.

 >>> runState (put 1) 0
 ((),1)
-}
put :: s -> State s ()
put x = State $ const ((), x)

{- | Implement the `Functor` instance for `State s`.

-- > runState ((+1) <$> State (\s -> (9, s * 2))) 3
 (10,6)
-}
instance Functor (State s) where
    (<$>) :: (a -> b) -> State s a -> State s b
    (<$>) f (State lhs) = State next
        where
            next s = (f a, s')
                where
                    (a, s') = lhs s

{- | Implement the `Applicative` instance for `State s`.

 >>> runState (pure 2) 0
 (2,0)

 >>> runState (pure (+1) <*> pure 0) 0
 (1,0)

-- > runState (State (\s -> ((+3), s ++ ("apple":.Nil))) <*> State (\s -> (7, s ++ ("banana":.Nil)))) Nil
 (10,["apple","banana"])
-}
instance Applicative (State s) where
    pure :: a -> State s a
    pure x =
        State (\s -> (x, s))
    (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) (State context) (State lhs) = State next
        where
            next s = (f a, s'')
                where
                    -- Question: does the order matter here? Should the context state happen first,
                    -- or should the lhs state?
                    --
                    -- Is it the case that since these are pure functions, this doesn't matter?
                    (a, s') = lhs s
                    (f, s'') = context s'

{- | Implement the `Monad` instance for `State s`.

 >>> runState ((const $ put 2) =<< put 1) 0
 ((),2)

-- > let modify f = State (\s -> ((), f s)) in runState (modify (+1) >>= \() -> modify (*2)) 7
 ((),16)

-- > runState ((\a -> State (\s -> (a + s, 10 + s))) =<< State (\s -> (s * 2, 4 + s))) 2
 (10,16)
-}
instance Monad (State s) where
    (=<<) :: (a -> State s b) -> State s a -> State s b
    -- After implementing this, I saw a more terse example using runState.
    -- TODO, maybe if I have time: come back to this and compose runState instead
    -- of implementing from scratch (although this was fun to figure out).
    (=<<) f (State lhs) = State next
        where
            next s = generated s'
                where
                    (a, s') = lhs s
                    (State generated) = f a

{- | Find the first element in a `List` that satisfies a given predicate.
 It is possible that no element is found, hence an `Optional` result.
 However, while performing the search, we sequence some `Monad` effect through.

 Note the similarity of the type signature to List#find
 where the effect appears in every return position:
   find ::  (a ->   Bool) -> List a ->    Optional a
   findM :: (a -> f Bool) -> List a -> f (Optional a)

-- > let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
 (Full 'c',3)

-- > let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
 (Empty,8)
-}
findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
findM _ Nil = pure Empty
findM p (x :. xs) = do
    keep <- p x
    if keep then pure (Full x) else (findM p xs)

{- | Find the first element in a `List` that repeats.
 It is possible that no element repeats, hence an `Optional` result.

 /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.

 -- findM :: Monad f => (a -> f Bool) -> List a -> f (Optional a)
 -- findM :: (a -> State (Set a) Bool) -> List a -> State (Set a) (Optional a)

-- > firstRepeat $ 1 :. 2 :. 0 :. 9 :. 2 :. 1 :. Nil
 Full 2

 prop> \xs -> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
 prop> \xs -> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
-}
firstRepeat :: forall a. Ord a => List a -> Optional a
firstRepeat xs = eval (findM mp xs) S.empty
    where
        mp x = do
            seen <- get
            if S.member x seen
              then pure True
              else do
                  put $ S.insert x seen
                  pure False

{- | Remove all duplicate elements in a `List`.
 /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.

-- > distinct $ 1 :. 1 :. 2 :. 3 :. 2 :. Nil

 prop> \xs -> firstRepeat (distinct xs) == Empty

 prop> \xs -> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
-}
distinct :: Ord a => List a -> List a
distinct xs = eval (filtering mp xs) S.empty
    where
        mp x = do
            seen <- get
            if S.member x seen
              then pure False
              else do
                  put $ S.insert x seen
                  pure True

{- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
 In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
 because it results in a recurring sequence.

 /Tip:/ Use `firstRepeat` with `produce`.

 /Tip:/ Use `join` to write a @square@ function.

 /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.

-- > isHappy 4
 False

-- > isHappy 7
 True

-- > isHappy 42
 False

-- > isHappy 44
 True
-}
isHappy :: Int -> Bool
isHappy x = contains 1 $ firstRepeat squares
    where
        digits y = digitToInt <$> (listh $ show y)
        square y = foldLeft step init list
            where
                step acc next = acc + next * next
                init = 0
                list = digits y
        squares = produce square x

-- $> test test_State
