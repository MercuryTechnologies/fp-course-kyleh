{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Course.FileIO where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: FilePath -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Tuple Functions that could help --

  fst :: (a, b) -> a
  snd :: (a, b) -> b
  (,) :: a -> b -> (a, b)

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Consideration --
  Try to avoid repetition. Factor out any common expressions.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

\$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- Given the file name, and file contents, print them.
-- Use @putStrLn@.
printFile :: FilePath -> Chars -> IO ()
printFile fp content = putStrLn $ "Path: " ++ fp ++ "\nContent: " ++ content

-- Given a list of (file name and file contents), print each.
-- Use @printFile@.
printFiles :: List (FilePath, Chars) -> IO ()
printFiles xs =
  let
    outputs = (\(fp, content) -> printFile fp content) <$> xs
  in
    void . sequence $ outputs


-- Given a file name, return (file name and file contents).
-- Use @readFile@.
getFile :: FilePath -> IO (FilePath, Chars)
getFile fp = do
  content <- readFile fp
  pure (fp, content)

-- Given a list of file names, return list of (file name and file contents).
-- Use @getFile@.
getFiles :: List FilePath -> IO (List (FilePath, Chars))
getFiles xs = sequence (getFile <$> xs)

-- Given a file name, read it and for each line in that file, read and print contents of each.
-- Use @getFiles@, @lines@, and @printFiles@.
run :: FilePath -> IO ()
run fp = do
  (_, names) <- getFile fp
  paths <- getFiles (lines names)
  printFiles paths

-- /Tip:/ use @getArgs@ and @run@
main :: IO ()
main = do
  (fp:._) <- getArgs
  run $ fp

----

-- Was there was some repetition in our solution?
-- ? `sequence . (<$>)`
-- ? `void . sequence . (<$>)`
-- Factor it out.
