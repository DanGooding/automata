
module NFAe
  ( NFAe (..)
  , consume
  , accepts
  , toDFA
  ) where

import Data.List (intersect, union, sort)

import qualified DFA

data NFAe a q = NFAe { states :: [q]
                     , transition :: q -> a -> [q]
                     , epsilonTransition :: q -> [q]
                     , start :: q
                     , accepting :: [q]
                     }

-- take epsilon transitions until have all reachable states
epsilonClosure :: (Eq q) => NFAe a q -> [q] -> [q]
epsilonClosure m qs
  | length qs == length qs' = qs                    -- no new elements added in one step - found all reachable
  | otherwise               = epsilonClosure m qs'  -- new elements added in one step, try another step
    where qs' = qs `union` (qs >>= epsilonTransition m)
-- do a DFS / BFS ? (the above is inefficient)

consume :: (Eq q) => NFAe a q -> [q] -> [a] -> [q]
consume m qs [] = epsilonClosure m qs
consume m qs (x:xs) = consume m qs' xs
  where qs' = epsilonClosure m qs >>= \ q -> transition m q x

accepts :: (Eq q) => NFAe a q -> [a] -> Bool
accepts m xs = not $ null $ consume m [start m] xs `intersect` accepting m

-- the powerset construction
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = p ++ map (x:) p
  where p = powerset xs

toDFA :: (Ord q) => NFAe a q -> DFA.DFA a [q]
toDFA m = DFA.DFA
  { DFA.states = p
  , DFA.transition = \ qs x -> sort $ consume m qs [x]    -- sort so list equality == set equality
  , DFA.start = epsilonClosure m [start m]
  , DFA.accepting = filter (not . null . intersect (accepting m)) p
  } where p = powerset $ states m
