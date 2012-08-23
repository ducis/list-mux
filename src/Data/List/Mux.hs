-- | This library provides functions for list multiplexing.
module Data.List.Mux
    ( -- * Multiplex two lists starting from the left element
      mux
      -- * ... or the right element
    , mux'
    )
  where

-- | Two examples follow below:
--
-- >>> mux [1,2,3] [4,5,6]
-- [1,4,2,5,3,6]
--
-- >>> mux' [1,2,3] [4,5,6]
-- [4,1,5,2,6,3]
--

mux :: [a] -> [a] -> [a]
mux = muxl

mux' :: [a] -> [a] -> [a]
mux' = muxr

muxl :: [a] -> [a] -> [a]
muxl [] [] = []
muxl xs [] = xs
muxl [] ys = ys
muxl (x:xs) ys = x : muxr xs ys

muxr :: [a] -> [a] -> [a]
muxr [] [] = []
muxr xs [] = xs
muxr [] ys = ys
muxr xs (y:ys) = y : muxl xs ys
