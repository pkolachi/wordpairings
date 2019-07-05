module MinSat where

import Data.Map as M

mini :: Ord a => [[a]] -> [a]
mini [] =
  []

mini cls =
  case [ x | [x] <- cls ] of
    x:_ -> x : mini (set x cls)
    _   -> x : mini (set x cls)
   where
    mp = M.fromListWith (+) [ (x,1) | c <- cls, x <- c ]
    x  = snd $ maximum [ (n,x) | (x,n) <- M.toList mp ]

set :: Ord a => a -> [[a]] -> [[a]]
set x cls = [ c | c <- cls, x `notElem` c ]
