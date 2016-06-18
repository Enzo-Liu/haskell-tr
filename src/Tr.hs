-- | Haskell tr implementation. Just supports the swap and delete modes:
-- * tr string1 string2
-- * tr -d string1
--
-- PLEASE DON'T CHANGE THE INTERFACE OF THIS FILE AS WE WILL EXPECT IT TO BE
-- THE SAME WHEN TESTING!
module Tr
    ( CharSet
    , tr
    ) where

import           Data.List (elemIndex)

-- | Just to give `tr` a more descriptive type
type CharSet = String

-- | 'tr' - the characters in the first argument are translated into characters
-- in the second argument, where first character in the first CharSet is mapped
-- to the first character in the second CharSet. If the first CharSet is longer
-- than the second CharSet, the last character found in the second CharSet is
-- duplicated until it matches in length.
--
-- If the second CharSet is a `Nothing` value, then 'tr' should run in delete
-- mode where any characters in the input string that match in the first
-- CharSet should be removed.
--
-- The third argument is the string to be translated (i.e., STDIN) and the
-- return type is the output / translated-string (i.e., STDOUT).
--
-- translate mode: tr "eo" (Just "oe") "hello" -> "holle"
-- delete mode: tr "e" Nothing "hello" -> "hllo"
--
-- It's up to you how to handle the first argument being the empty string, or
-- the second argument being `Just ""`, we will not be testing this edge case.

tr :: CharSet -> Maybe CharSet -> String -> String
tr is Nothing = trd is
tr _ (Just "") = id
tr is (Just os) = trs is os

trs :: CharSet -> CharSet -> String -> String
trs is os | li > lo = trs is $ os ++ replicate diff (last os)
          where li = length is
                lo = length os
                diff = li - lo

trs is os = map swap
  where swap x | Just i <- elemIndex x is = os !! i
               | otherwise = x

trd :: CharSet -> String -> String
trd is = filter (`notElem` is)
