module Fibonacci where

import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

s1, s2 :: Diagram B
s1 = square 1 # fc (sRGB 0.4 0.4 0.4)
s2 = rect 2 1 # fc lightgrey

path :: Int -> Diagram B
path n = mconcat
  [ rect (fromIntegral n) 1 # alignL
  , mconcat [vrule 1 # translateX (fromIntegral k) | k <- [1 .. n-1]]
  ]
  # dashingL [0.1,0.1] 0

paving :: [Int] -> Diagram B
paving = hcat . map pave
  where
    pave 1 = s1
    pave 2 = s2
