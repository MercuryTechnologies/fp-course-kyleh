{-# LANGUAGE NoImplicitPrelude #-}

-- | Course.Optional
--
-- We've stubbed out a bunch of functions for you to implement.
--
-- > mapOptional :: (a -> b) -> Optional a -> Optional b
-- > mapOptional = error "todo: Course.Optional#mapOptional"
--
-- This module comes equipped with unit tests!
-- To run the tests, start the file watcher, `ghcid` from the project root directory.
--
-- >  ~/../fp-course $ ghcid
--
-- `ghcid` will recompile your files on every save.
-- If the compilation succeeds, it will run the tests.
module Course.Optional where

import Control.Applicative qualified  as A
import Control.Monad qualified  as M
import Course.Core
import Prelude (String)
--              ^^^^^^ `String` is now in top-level global scope
--     ^^^^^^^ import `Prelude` once, unqualified
import Prelude qualified as P
--                          ^ everything in `Prelude` is now in scope, in the namespace `P`.
--     ^^^^^^^^^^^^^^^^^ import `Prelude` again, qualified this time

type Optional :: Type -> Type
--               ^^^^^^^^^^^^ `Optional` is a type constructor.
--                            If `Foo` is some type, then appling `Optional` to `Foo` results in the type `Optional Foo`.

-- | The `Optional` data type contains 0 or 1 value.
--
-- It might be thought of as a list, with a maximum length of one.
data Optional a = Empty | Full a
--                             ^ one constructor argument, type `a`
--                        ^^^^ second data constructor
--                ^^^^^ first data constructor. zero constructor arguments
--            ^ type variable
--   ^^^^^^^^ type name
  deriving (Eq, Show)
  --            ^^^^ compiler generates instance `Show a => Show (Optional a)`
  --        ^^ compiler generates instance `Eq a => Eq (Optional a)`

contains :: Eq a => a -> Optional a -> Bool
--                  ^ constrained type variable
--          ^^^^ constraint: `Eq a` must be true
--               this ensures that `(==)` is defined for signature `a -> a -> Bool`.
contains _ Empty = False
contains a1 (Full a2) = a1 == a2
--                      ^^^^^^^^ compare for equality
--                ^^ data carried by the `Full` constructor. `a2 :: a`
--       ^^ first argument. `a1 :: a`

-- | Question Optional 1
--
-- The `Optional` type we are defining here is analogous to Haskell's base library `Maybe` type.
-- Find `Maybe` on Hoogle, and navigate to its source.
-- (WARNING: Hoogle search is CasE SEnsItiVe!)
--     a) What are the data constructors of `Maybe`? What are their types?
--     b) What instances does `Maybe` derive? In your answer, state the instance context and the instance head.
question_Optional_1 :: (String, String)
question_Optional_1 =
  ( "The data constructors of maybe are `Just a` or `Nothing` which are both of type `Maybe a`."
  , "`Maybe` derives both `Eq` and `Ord` with `instance Eq a => Eq (Maybe a)` and `instance Ord a => Ord (Maybe a)`."
  )

-- | Return the possible value if it exists; otherwise, the first argument.
--
-- >>> fullOr 99 (Full 8)
-- 8
--
-- >>> fullOr 99 Empty
-- 99
--
-- The `base` package version is called `fromMaybe`, defined in `Data.Maybe`.
fullOr :: a -> Optional a -> a
fullOr _ (Full val) = val
fullOr unless Empty = unless

-- | Map the given function on the possible value.
--
-- >>> mapOptional (+1) Empty
-- Empty
--
-- >>> mapOptional (+1) (Full 8)
-- Full 9
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional f (Full x) = Full (f x)
mapOptional _ Empty = Empty

-- | Question Optional 2
--
-- Look back at `src/Course/ExactlyOne.hs` and `src/Course/Validation.hs`.
-- Specifically, look closely at `mapExactlyOne` and `mapValidation`.
--     a) Is there a pattern that all of their type signatures fall into?
--     b) Are there similarities in their implementations? Explain.
question_Optional_2 :: (String, String)
question_Optional_2 =
  ( "Yes, the signature for each map is roughly the same, only differing in the name of the type in the map signature."
  , "There are also equivalencies in the implementations, up to cardinality of data constructors. By that, I mean that the implementation for type `Foo` is usually `mapFoo f (SubFoo x) = SubFoo (f x)` for each data constructor `SubFoo` in `Foo`. There will be further differences given the needs/arguments for each data constructor (like how `Error s` disregards the function)."
  )

-- | Bind the given function on the possible value.
--
-- >>> bindOptional Full Empty
-- Empty
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 8)
-- Full 7
--
-- >>> bindOptional (\n -> if even n then Full (n - 1) else Full (n + 1)) (Full 9)
-- Full 10
bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional f (Full x) = f x
bindOptional _ Empty = Empty

-- | Question Optional 3
--
-- Look back at `src/Course/ExactlyOne.hs` and `src/Course/Validation.hs`.
-- Look back at `bindExactlyOne` and `bindValidation`.
--     a) Is there a pattern that all of their type signatures fall into?
--     b) Are there similarities in their implementations? Explain.
question_Optional_3 :: (String, String)
question_Optional_3 =
  ( "Much like in `map`, `bind` also follows the same type signature pattern except this time the provided function already wraps the return type."
  , "This difference means that the implementaiton is the same, but now we can omit wrapping the final value when calling `f` where we previously had to in `map`."
  )

-- | Try the first optional for a value. If it has a value, use it; otherwise,
-- use the second value.
--
-- >>> Full 8 <+> Empty
-- Full 8
--
-- >>> Full 8 <+> Full 9
-- Full 8
--
-- >>> Empty <+> Full 9
-- Full 9
--
-- >>> Empty <+> Empty
-- Empty
(<+>) :: Optional a -> Optional a -> Optional a
(<+>) optionalLeft@(Full _) _ = optionalLeft
(<+>) Empty optionalRight = optionalRight

-- | Apply the callback or else use the fallback.
--
-- >>> optional (+1) 0 (Full 8)
-- 9
--
-- >>> optional (+1) 0 Empty
-- 0
--
-- The `base` package version is called `maybe`, defined in `Data.Maybe`.
optional :: (a -> b) -> b -> Optional a -> b
--                      ^ fallback
--          ^^^^^^^^ callback
optional f _ (Full x) = f x
optional _ fallback Empty = fallback

-- | Question Optional 4
--
-- Why does the fallback in `optional` have type `b` rather than type `a`?
question_Optional_4 :: String
question_Optional_4 = ""

-- | Question Optional 5
--
-- Hoogle `maybe`. Read its documentation and navigate to its source.
--     a) How are `maybe` and `optional` similar?
--     b) How are `maybe` and `optional` different?
question_Optional_5 :: (String, String)
question_Optional_5 =
  ( "`maybe` and `optional` do the same thing, which is to allow unwrapping of an optional type into either its inner value or a fallback value in the case of an empty container."
  , "`maybe` has the fallback value as the first argument instead of the second."
  )

-- $noteToTrainee
--
-- We're done here!
-- Hereafter in this course, we'll use `Maybe` instead of `Optional`.
-- Move on to [Course.List](../List.hs)

instance P.Functor Optional where
    fmap = M.liftM

instance A.Applicative Optional where
    (<*>) = M.ap
    pure = Full

instance P.Monad Optional where
    (>>=) = flip bindOptional

-- That last line is what causes `ghcid` to run the tests, in case you were curious.

-- $> test test_Optional
