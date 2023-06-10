{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Course.List
--
-- + Complete the exercises below by filling out the function bodies.
--   Replace the function bodies (error "todo: ...") with an appropriate
--   solution.
--
-- + These exercises may be done in any order, however:
--   Exercises are generally increasing in difficulty, though some people may find later exercise easier.
--
-- + Bonus for using the provided functions, `foldRight` and `foldLeft`,
--   or for using one of your solutions to help solve another.
module Course.List where

import Control.Applicative qualified as A
import Control.Monad qualified as M
import Course.Core
import Course.Optional
import Numeric qualified as N
import System.Environment qualified as E
import Prelude (String)
import Prelude qualified as P

type List :: Type -> Type
--                   ^^^^ and returns the type `List a`.
--           ^^^^ takes a type variable `a`
--   ^^^^ The type constructor `List`

-- | A list of values that are all the same type.
--
-- 'List' has two data constructors:
--
-- > Nil  :: forall t.                List t
-- > (:.) :: forall t. t -> List t -> List t
--
-- So a list is either 'Nil', which has no fields,
-- or it's `(:.) x y`, whose first field, `x :: a`, is the first element of the list
-- and whose second field, `y :: List a`, is the rest of the list.
--
-- When pattern matching, we can name the fields whatever we like,
-- so it's tradition to name them `(:.) x xs`,
-- in order to make it easy to remember that `x` is a list element
-- and `xs` is the rest of the list.
-- I suggest you follow this convention, too. Most Haskell code does.
-- But they're variable names, so they can be whatever makes sense to you.
--
-- In the examples above, we wrote the constructor `(:.)` in prefix position.
-- In the code, we'll write `(:.)` in infix position,
-- but we could have written it in prefix position in the code, if we'd wanted.
--
-- > data List t = Nil | (:.) t (List t)
--
-- The appeal of using infix notation is that it requires fewer parentheses.
-- Haskellers tend to like fewer parentheses.
data List t = Nil | t :. List t
--                       ^^^^^^ type of second argument
--                  ^ type of first argument
--                    ^^ second data constructor. is symbolic, so infix.
--            ^^^ first data constructor, zero arguments
--        ^ type variable
--   ^^^^ type name
  deriving (Eq, Ord)
--              ^^^ compiler generates instance `Ord a => Ord (List a)`
--          ^^ compiler generates instance `Eq a => Eq (List a)`

infixr 5 :.
-- ^^^^^^^^ Fixity and precedence declaration for `:.`.
--          This is optional; the compiler has a default if you don't write one.
--          But it's usually a good idea to write a declaration exlicity for operators you define.
--     ^ Precedence 5 means that this operation bind before operations with a lower precedence.
--       For example, when we define `(++)`, we'll give it a precedence of 4.
--       `x :. xs ++ ys` will be parsed as `(x :. xs) ++ ys`, because `:.` has higher precedence than `(++)`.
--       Think of "order of operations" in arithmetic. `*` has higher precedence than `+`, so `2 + 3 * 4` is `14`, not `10`.
--       Similarly, `x :. xs ++ ys` is `x :. (xs ++ ys)`, not `(x :. xs) ++ ys`.
-- ^^^ The "r" in `infixr` means `x :. y :. z :. zs` is parsed as `x :. (y :. (z :. zs))`.
--     Parentheses start at the right-most `:.`.
--     For operations like addition this wouldn't matter, because `2 + (3 + (4 + 5))` and `((2 + 3) + 4) + 5` are the same.
--     Operations where the order doesn't matter are called "associative" operations.
--     For `:.`, the order does matter: `:.` is non-associative.
--     Imagine that each of `x`, `y`, and `z` is a list element and `zs` is a list.
--     If we had choosen `infixl` instead of `infixr`, then `x :. y :. z :. zs` would be a type error.
--     The compiler would insert parentheses as `((x :. y) :. z) :. zs`, but the result is not well-typed.
--     So we chose `infixr` purely for convenience, so that we can write `x :. y :. z :. zs` without parentheses.

-- | Question List 1
--
--   (a) Using the notation above, why isn't `(x :. y) :. z :. zs` well-typed?
--   (b) If we had chosen `infixl` instead of `infixr`, what would have have to write instead of `x :. y :. z :. zs`?
question_List_1 :: (String, String)
question_List_1 =
  ( "This is because `(x :. y)` is trying to concatenate `x` and `y`, but `y` may not be a `List t`, which is neccessary given the definition of the type."
  , "`(x :. (y :. (z :. zs)))`"
  )

-- | The instance 'Show (List t)' defines its 'show' function to first convert a @'List' t@ into a @[t]@.
-- Then, it uses the 'show' function from the instance 'Show [t]' to produce a 'String'.
instance Show t => Show (List t) where
    show = show . hlist
    --            ^^^^^ converting a `List t` to a `[t]`.
    --          ^ to the result of...
    --     ^^^^ apply the `show` function from instance `Show [t]`...
    --   ^ is defined as...
    -- ^ The `show` function from instance `Show (List t)`...

-- | Question List 2
--
-- Our 'List' type is analogous to the '[]' type in Haskell's base library.
--
-- > data [a] = [] | a : [a]
-- > --                ^ "cons" list data constructor
-- > --         ^^ empty list data constructor
--
-- Instead of @List a@, we have @[a]@.
-- Instead of @Nil@, we have @[]@.
-- Instead of @(:.)@, we have @(:)@.
--
-- Use Hoogle or GHCi to answer the following questions.
--
--   (a) What is the type of @(:)@?
--   (b) What are the fixity and precedence of @(:)@.
--   (d) What is the type of @[]@?
--   (c) What is the kind of @[]@?
--
-- Free response.
--
--   (e) Are the @[]@ and @[]@ in questions (c) and (d) the same? How are they different?
question_List_2 :: (String, String, String, String, String)
question_List_2 =
  ( "`:` is of type `a -> [a] -> [a]` which is from a generic type, to a list of that type, again resulting in a list of that type."
  , "I wasn't able to find info on the fixity of `:`, but assuming it has the same fixity as `:.` of 5, that means that `:` will be evaluated before Operators like `(++)`."
  , "`[]` is of type `[a]` where `a` is a free type parameter."
  , "The kind of `[]` is `* -> *`"
  , "They are not the same. In `:type []`, `[]` refers to a value and in `:kind []`, `[]` refers to a type constructor with one type argument."
  )

-- | The list of integers from zero to infinity.
infinity :: List Integer
infinity =
    let inf x = x :. inf (x + 1)
     in inf 0

-- $listutilities
--
-- 'List' utility function that may consider using.

-- | Process a list, parenthesizing from the right.
--
-- > foldRight f z0 (x1 :. x2 :. x3 :. Nil)
--
-- evaluates to
--
-- > f x1 (f x2 (f x3 z0))
foldRight :: (a -> b -> b) -> b -> List a -> b
--                                           ^ the final accumulator.
--                                 ^^^^^^ the list to be processed.
--                            ^ the initial accumulator.
--           ^^^^^^^^^^^^^ a callback to apply with each list element
--                         The `a` argument is the current list element.
--                         The `b` argument is the current accumulator.
--                         The result is the next accumulator.
foldRight _ b Nil = b
foldRight f b (h :. t) = f h (foldRight f b t)

-- | Process a list, parenthesizing from the left.
--
-- > foldLeft f z0 (x1 :. x2 :. x3 :. Nil)
--
-- evaluates to
--
-- > f (f (f z0 x1) x2) x3
foldLeft :: (b -> a -> b) -> b -> List a -> b
foldLeft _ b Nil = b
foldLeft f b (h :. t) =
  let
    b' = f b h
  in
    b' `seq` foldLeft f b' t

-- | Question List 3
--
-- Remember the fixity of @(:.)@.
--
-- Notes from our chat:
--
-- Example list:
-- existing = (x1 :. (x2 :. (x3 :. Nil)))
--
-- Examples of evaluation order from Daniel:
-- foldRight (&) z0 (x1 :. x2 :. x3 :. Nil) = x1 & (x2 & (x3 & z0))
-- foldLeft  (&) z0 (x1 :. x2 :. x3 :. Nil) = ((z0 & x1) & x2) & x3
--
-- Kyle working out evaluation of both foldl and foldr:
-- [x1]
-- ((Nil & x1) & x2) & x3
-- ([x1] & x2) & x3
-- [x2, x1] & x3
-- [x3, x2, x1]
--
-- ((z0 * x1) * x2) * x3
--
-- (x :. (y + z))
--
-- (1 o (2 o (3 o Nil)))
--           ^^^^^^^^^
--      ^^^^^^^^^^^^^^^
-- ^^^^^|^^^^^^^^^^^^^^^
-- |    |
-- 1    2    3 Nil
--
-- ((1 f' 2) f' 3)
-- 3 2 1
--
--   (a) If you wanted to end up with `x1 :. x2 :. x3 :. Nil` using `foldRight f z0`,
--       what would you use for `f` and `z0`?
--   (b) When you use the same `f` and `z0` in `foldLeft f z0`, you get a type error, why?
--   (c) Using a lambda your `f`, and without using any additional functions,
--       define a function `f'` so that `foldLeft f' z0` will type check.
--   (d) What is the result of `foldLeft f' z0 (1 :. 2 :. 3 :. Nil)`? Explain.
question_List_3 :: (String, String, String, String)
question_List_3 =
  ( "Assuming `existing = x1 :. x2 :. x3 :. Nil`, you could get the same back by making an identity transform with `foldRight (:.) Nil`."
  , "`foldLeft (:.) Nil` doesn't fit the type signature required for `foldLeft`. The cons operator `:.` is handed and expects to have an element of type `a` on the left and a `List a` on the right. The first argument to `foldLeft` needs to instead be of type `b -> a -> b`."
  , "To make the expression type check correctly, the arguments need to be flipped (at least this is one way to make it work). One example would be `foldLeft (\\u v -> v :. u) Nil`. Alternatively, without a lambda, you could do `foldLeft (flip (:.)) Nil`."
  , "The result of that expression would be `[3,2,1]` which would be the result of `(f' (f' (f' Nil 3) 2) 1)`"
  )

-- | Question List 4
--
-- `foldRight` and `foldLeft` here are analogous to `foldr` and `foldl'` in Haskell's base library.
-- Haskell's base library also has a `foldl` function.
-- Use Hoogle to find the implementations of `foldl` and `foldl'` for `[]`.
--
--   (a) What's the implementation of `foldl`?
--   (b) What's the implementation of `foldl'`?
--   (c) What's the difference? (You don't have to _understand_ the difference yet, just spot it.)
question_List_4 :: (String, String, String)
question_List_4 =
  ( "`foldl` is implemented as  `foldr (\\(v::a) (fn::b->b) -> oneShot (\\(z::b) -> fn (k z v))) (id :: b -> b) xs z0`"
  , "`foldl'` is implemented as `foldr (\\(v::a) (fn::b->b) -> oneShot (\\(z::b) -> z `seq` fn (k z v))) (id :: b -> b) xs z0`"
  , "`foldl'` uses `seq` in order to inform the compiler that a wrapping thunk should not be created for the intermediate return call, which makes `fn` the last call on the stack of each fold allowing for much better optimization."
  )

-- $listexercises

-- | Returns the first element of the list, or the given default in case the list is empty.
--
-- > headOr 3 (1 :. 2 :. Nil)
-- 1
--
-- > headOr 3 Nil
-- 3
headOr :: a -> List a -> a
headOr x Nil = x
headOr _ (x :. _) = x

-- | Question List 5
--
--   (a) `headOr` can be implemented using either pattern matching or `foldRight`.
--       Whichever you used in the exercise, implement it using the other here.
--   (b) You can also implement `headOr` using `foldLeft`. Implement it here.
--   Note from meeting: we think that (b) could include a hint/leading question on using something extra for `foldLeft` impl.
--   (c) Which takes less time to run `headOr (1 :. ... :. 10 :. Nil)`,
--       implementing using `foldLeft` or `foldRight`? Explain.
headOr' :: a -> List a -> a
headOr' fb xs = foldRight (\a _ -> a) fb xs
-- > headOr' 3 (1 :. 2 :. Nil)
-- 1
--
-- > headOr' 3 Nil
-- 3

headOr'' :: a -> List a -> a
headOr'' fb xs = optional id fb (foldLeft (\a b -> a <+> Full b) Empty xs)
-- > headOr'' 3 (1 :. 2 :. 3 :. 4 :. Nil)
-- 1
--
-- > headOr'' 3 Nil
-- 3

question_List_5 :: (String, String, String)
question_List_5 =
  ( "(example headOr' above)"
  , "(example headOr'' above)"
  , "`foldRight` will take much less time since it doesn't care about the value of the rest of the list (it's represented by a hole), and `foldLeft` will need to go through the whole list before returning the final result."
  )

-- | The product of the elements of a list.
--
-- > product Nil
-- 1
--
-- > product (1 :. 2 :. 3 :. Nil)
-- 6
--
-- > product (1 :. 2 :. 3 :. 4 :. Nil)
-- 24
--
-- Implement this function using `foldLeft`.
product :: List Int -> Int
product xs = foldLeft (*) 1 xs

-- | Sum the elements of the list.
--
-- > sum (1 :. 2 :. 3 :. Nil)
-- 6
--
-- > sum (1 :. 2 :. 3 :. 4 :. Nil)
-- 10
--
-- Implement this function using `foldLeft`.
sum :: List Int -> Int
sum xs = foldLeft (+) 0 xs

-- | Return the length of the list.
--
-- > length (1 :. 2 :. 3 :. Nil)
-- 3
--
-- Implement this function using `foldLeft`.
length :: List a -> Int
length xs = foldLeft (\acc _ -> acc + 1) 0 xs

-- | Map the given function over a list.
--
-- > map (+10) (1 :. 2 :. 3 :. Nil)
-- [11,12,13]
--
-- Implement this function using pattern matching.
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (x :. xs) = f x :. map f xs

-- | Question List 6
--
--   (a) Is it possible for `map` to return a list that is longer than the input list? How/Why?
--   (b) Is it possible for `map` to return a list that is shorter than the input list? How/Why?
question_List_6 :: (String, String)
question_List_6 =
  ( "Nope, the mapping function can only return one value. If this was `flatMap`, then more elements could be returned."
  , "Nope, the mapping funciton has to return at least one value."
  )

-- | Question List 7
--
-- Haskell supports a feature called "partial application".
-- This means that we can write `map f` and it would mean `\xs -> map f xs`.
--
--   (a) If `f :: X -> Y`, what's the type of `map f`?
--   (b) Some people describe the `map` function as
--       "lifting an operation on elements to an operation on lists".
--       In your own words, what does this mean?
question_List_7 :: (String, String)
question_List_7 =
  ( "`map f` would be of type `List X -> List Y`."
  , "Lifting takes a simple operation and brings it into the `List` structure without needing to require definition of a new function."
  )

-- | Question List 8
--
-- Work out these questions on paper with pen or pencil.
-- I'd prefer that you didn't use GHCi to answer any of these,
-- but if you spend more than 2.5 minutes on any one question,
-- ask GHCi for the answer, convince yourself why it's right, and continue.
--
--   (a) What is the type of `map . map`? What does it do?
--   (b) What is the type of `map map`? What does it do?
--   (c) What is the type of `map . mapOptional`? What does it do?
--   (d) What is the type of `mapOptional . map`? What does it do?
--   (e) What is the type of `mapOptional map`? What does it do?
--   (f) What is the type of `map mapOptional`? What does it do?
question_List_8 :: (String, String, String, String, String, String)
question_List_8 =
  ( "`map . map` is of type `(a -> b) -> List (List a) -> List (List b)` and instead maps `f` onto a list of lists."
  , "`map map` is of type `List (a -> b) -> List (List a -> List b)` which takes a list of functions and returns a list of lifted functions."
  , "`map . mapOptional` is of type `(a -> b) -> List Optional a -> List Optional b` which takes a function, lifts it into the `Optional` type, then lifts it into the `List` type to let you map a function over a list of optional values."
  , "`mapOptional . map` is of type `(a -> b) -> Optional List a -> Optional List b` which lifts a function `f` into a mapper over lists which may or may not exist as an optional (`Optional List a`)."
  , "`mapOptional map` is of type `Optional (a -> b) -> Optional (List a -> List b)` which takes an optional function and returns an optional lifted version of that function."
  , "`map mapOptional` is of type `List (a -> b) -> List (Optional a -> Optional b)` which takes a list of functions and creates a list of functions lifted into `Optional`."
  )

-- QUICKSAVE

-- | Return elements satisfying the given predicate.
--
-- > filter even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
-- [2,4]
--
-- Implement this function using pattern matching.
filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter f (x :. xs) =
  let
    rest = filter f xs
  in
    if (f x) then (x :. rest) else rest 

-- | Question List 9
--
--   (a) Implement `map` using `foldRight`.
--   (b) Implement `filter` using `foldRight`.
--
-- > map' (3+) (1 :. 2 :. 3 :. Nil)
-- [4,5,6]
--
-- > filter' odd (1 :. 2 :. 3 :. Nil)
-- [1,3]
map' :: (a -> b) -> List a -> List b
map' f xs = foldRight step init list
  where
    step = \next acc -> (f next) :. acc
    init = Nil
    list = xs

filter' :: (a -> Bool) -> List a -> List a
filter' f xs = foldRight step init list
  where
    step = \next acc -> if (f next) then (next :. acc) else acc
    init = Nil
    list = xs

question_List_9 :: (String, String)
question_List_9 =
  ( "see `map'`"
  , "see `filter'`"
  )

-- | Append two lists to a new list.
--
-- > (1 :. 2 :. 3 :. Nil) ++ (4 :. 5 :. 6 :. Nil)
-- [1,2,3,4,5,6]
--
-- Implement this function using `foldRight`.
--
-- Daniel shared a great technique on how to set myself up for
-- thinking clearly about folds. Start by creating scaffolding using a `where` block
-- to stub out the pieces of the fold. This allows for better isolation of concepts
-- to be filled out, rather than trying to understand everything at once.
-- (++) xs ys = foldRight step init list
--   where
--     step next acc = undefined
--     init = undefined
--     list = undefined
(++) :: List a -> List a -> List a
(++) xs ys = foldRight (:.) ys xs
-- (++) xs ys = foldRight step init list
  -- where
  --   step next acc = next :. acc
  --   init = ys
  --   list = xs

infixr 5 ++

-- | Flatten a list of lists to a list.
--
-- > flatten ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
flatten :: List (List a) -> List a
flatten = foldLeft (++) Nil

{- | Map a function then flatten to a list.

 > flatMap (\x -> x :. x + 1 :. x + 2 :. Nil) (1 :. 2 :. 3 :. Nil)
 [1,2,3,2,3,4,3,4,5]

 prop> \x -> headOr x (flatMap id (infinity :. y :. Nil)) == 0

 prop> \x -> headOr x (flatMap id (y :. infinity :. Nil)) == headOr 0 y

 prop> \x -> flatMap id (x :: List (List Int)) == flatten x
-}
flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = flatten (map f xs)

-- | Flatten a list of lists to a list (again).
-- HOWEVER, this time use the /flatMap/ function that you just wrote.
--
-- > flattenAgain ((1 :. 2 :. 3 :. Nil) :. (4 :. 5 :. 6 :. Nil) :. (7 :. 8 :. 9 :. Nil) :. Nil)
-- [1,2,3,4,5,6,7,8,9]
flattenAgain :: List (List a) -> List a
flattenAgain = flatMap id

{- | Convert a list of optional values to an optional list of values.

 * If the list contains all `Full` values,
 then return `Full` list of values.

 * If the list contains one or more `Empty` values,
 then return `Empty`.

 > seqOptional (Full 1 :. Full 10 :. Nil)
 Full [1, 10]

 > seqOptional Nil
 Full []

 > seqOptional (Full 1 :. Full 10 :. Empty :. Nil)
 Empty

 > seqOptional (Empty :. map Full infinity)
 Empty
-}
-- TODO: This could probably be implemented with fold
seqOptional :: List (Optional a) -> Optional (List a)
seqOptional Nil = Full Nil
seqOptional (Empty :. _) = Empty
seqOptional (Full x :. right) =
  case seqOptional right of
    Full rest -> Full (x :. rest)
    Empty -> Empty

{- | Find the first element in the list matching the predicate.

 > find even (1 :. 3 :. 5 :. Nil)
 Empty

 > find even Nil
 Empty

 > find even (1 :. 2 :. 3 :. 5 :. Nil)
 Full 2

 > find even (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
 Full 2

 > find (const True) infinity
 Full 0
-}
-- TODO: Doesn't work in the infinite case, can we use fold?
-- find :: (a -> Bool) -> List a -> Optional a
-- find f xs = foldLeft step init list
--   where
--     step = \acc next -> acc <+> if (f next) then Full next else Empty
--     init = Empty
--     list = xs
find :: (a -> Bool) -> List a -> Optional a
find _ Nil = Empty
find f (x :. xs) = if (f x) then Full x else find f xs

{- | Determine if the length of the given list is greater than 4.

 > lengthGT4 (1 :. 3 :. 5 :. Nil)
 False

 > lengthGT4 Nil
 False

 > lengthGT4 (1 :. 2 :. 3 :. 4 :. 5 :. Nil)
 True

 > lengthGT4 infinity
 True
-}
lengthGT4 :: List a -> Bool
lengthGT4 = error "derp"

{- | Reverse a list.

 > reverse (1 :. 2 :. Nil)
 []

 > take 1 (reverse (reverse largeList))
 [1]

 prop> \x -> let types = x :: List Int in reverse x ++ reverse y == reverse (y ++ x)

 prop> \x -> let types = x :: Int in reverse (x :. Nil) == x :. Nil
-}
reverse :: List a -> List a
reverse = foldLeft (flip (:.)) Nil

{- | Produce an infinite `List` that seeds with the given value at its head,
 then runs the given function for subsequent elements

 > let (x:.y:.z:.w:._) = produce (+1) 0 in [x,y,z,w]
 [0,1,2,3]

 > let (x:.y:.z:.w:._) = produce (*2) 1 in [x,y,z,w]
 [1,2,4,8]
-}
produce :: (a -> a) -> a -> List a
produce f init = init :. produce f (f init)

---- End of list exercises

largeList :: List Int
largeList =
    listh [1 .. 50000]

hlist :: List a -> [a]
hlist =
    foldRight (:) []

listh :: [a] -> List a
listh =
    P.foldr (:.) Nil

putStr :: Chars -> IO ()
putStr =
    P.putStr . hlist

putStrLn :: Chars -> IO ()
putStrLn =
    P.putStrLn . hlist

readFile :: FilePath -> IO Chars
readFile =
    P.fmap listh . P.readFile . hlist

writeFile :: FilePath -> Chars -> IO ()
writeFile n s =
    P.writeFile (hlist n) (hlist s)

getLine :: IO Chars
getLine =
    P.fmap listh P.getLine

getArgs :: IO (List Chars)
getArgs =
    P.fmap (listh . P.fmap listh) E.getArgs

isPrefixOf :: Eq a => List a -> List a -> Bool
isPrefixOf Nil _ =
    True
isPrefixOf _ Nil =
    False
isPrefixOf (x :. xs) (y :. ys) =
    x == y && isPrefixOf xs ys

isEmpty :: List a -> Bool
isEmpty Nil =
    True
isEmpty (_ :. _) =
    False

span :: (a -> Bool) -> List a -> (List a, List a)
span p x =
    (takeWhile p x, dropWhile p x)

break :: (a -> Bool) -> List a -> (List a, List a)
break p =
    span (not . p)

dropWhile :: (a -> Bool) -> List a -> List a
dropWhile _ Nil =
    Nil
dropWhile p xs@(x :. xs') =
    if p x
        then dropWhile p xs'
        else xs

takeWhile :: (a -> Bool) -> List a -> List a
takeWhile _ Nil =
    Nil
takeWhile p (x :. xs) =
    if p x
        then x :. takeWhile p xs
        else Nil

zip :: List a -> List b -> List (a, b)
zip =
    zipWith (,)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f (a :. as) (b :. bs) =
    f a b :. zipWith f as bs
zipWith _ _ _ =
    Nil

unfoldr :: (a -> Optional (b, a)) -> a -> List b
unfoldr f a =
    case f a of
        Full (b, a') -> b :. unfoldr f a'
        Empty -> Nil

lines :: Chars -> List Chars
lines =
    listh . P.fmap listh . P.lines . hlist

unlines :: List Chars -> Chars
unlines =
    listh . P.unlines . hlist . map hlist

words :: Chars -> List Chars
words =
    listh . P.fmap listh . P.words . hlist

unwords :: List Chars -> Chars
unwords =
    listh . P.unwords . hlist . map hlist

listOptional :: (a -> Optional b) -> List a -> List b
listOptional _ Nil =
    Nil
listOptional f (h :. t) =
    let r = listOptional f t
     in case f h of
            Empty -> r
            Full q -> q :. r

any :: (a -> Bool) -> List a -> Bool
any p =
    foldRight ((||) . p) False

all :: (a -> Bool) -> List a -> Bool
all p =
    foldRight ((&&) . p) True

or :: List Bool -> Bool
or =
    any id

and :: List Bool -> Bool
and =
    all id

elem :: Eq a => a -> List a -> Bool
elem x =
    any (== x)

notElem :: Eq a => a -> List a -> Bool
notElem x =
    all (/= x)

permutations :: List a -> List (List a)
permutations xs0 =
    let perms Nil _ =
            Nil
        perms (t :. ts) is =
            let interleave' _ Nil r =
                    (ts, r)
                interleave' f (y :. ys) r =
                    let (us, zs) = interleave' (f . (y :.)) ys r
                     in (y :. us, f (t :. y :. us) :. zs)
             in foldRight (\xs -> snd . interleave' id xs) (perms ts (t :. is)) (permutations is)
     in xs0 :. perms xs0 Nil

intersectBy :: (a -> b -> Bool) -> List a -> List b -> List a
intersectBy e xs ys =
    filter (\x -> any (e x) ys) xs

take :: (Num n, Ord n) => n -> List a -> List a
take n _
    | n <= 0 =
        Nil
take _ Nil =
    Nil
take n (x :. xs) =
    x :. take (n - 1) xs

drop :: (Num n, Ord n) => n -> List a -> List a
drop n xs
    | n <= 0 =
        xs
drop _ Nil =
    Nil
drop n (_ :. xs) =
    drop (n - 1) xs

repeat :: a -> List a
repeat x =
    x :. repeat x

replicate :: (Num n, Ord n) => n -> a -> List a
replicate n x =
    take n (repeat x)

reads :: P.Read a => Chars -> Optional (a, Chars)
reads s =
    case P.reads (hlist s) of
        [] -> Empty
        ((a, q) : _) -> Full (a, listh q)

read :: P.Read a => Chars -> Optional a
read =
    mapOptional fst . reads

readHexs :: (Eq a, Num a) => Chars -> Optional (a, Chars)
readHexs s =
    case N.readHex (hlist s) of
        [] -> Empty
        ((a, q) : _) -> Full (a, listh q)

readHex :: (Eq a, Num a) => Chars -> Optional a
readHex =
    mapOptional fst . readHexs

readFloats :: (RealFrac a) => Chars -> Optional (a, Chars)
readFloats s =
    case N.readSigned N.readFloat (hlist s) of
        [] -> Empty
        ((a, q) : _) -> Full (a, listh q)

readFloat :: (RealFrac a) => Chars -> Optional a
readFloat =
    mapOptional fst . readFloats

instance (a ~ Char) => IsString (List a) where
    -- Per https://hackage.haskell.org/package/base-4.14.1.0/docs/src/Data.String.html#line-43
    fromString =
        listh

type Chars =
    List Char

type FilePath =
    List Char

strconcat :: [Chars] -> P.String
strconcat =
    P.concatMap hlist

stringconcat :: [P.String] -> P.String
stringconcat =
    P.concat

show' :: Show a => a -> List Char
show' =
    listh . show

instance P.Functor List where
    fmap f =
        listh . P.fmap f . hlist

instance A.Applicative List where
    (<*>) =
        M.ap
    pure =
        (:. Nil)

instance P.Monad List where
    (>>=) =
        flip flatMap
