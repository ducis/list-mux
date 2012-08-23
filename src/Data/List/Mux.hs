module Data.List.Mux
    ( -- * Multiplex two lists starting from the left element
      mux
      -- * ... or the right element
    , mux'
    )
  where

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
