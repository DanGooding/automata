
module DFA
  ( DFA (..)
  , consume
  , accepts
  ) where

data DFA a q = DFA { states :: [q]
                   , transition :: q -> a -> q
                   , start :: q
                   , accepting :: [q]
                   }

consume :: DFA a q -> q -> [a] -> q
consume m = foldl (transition m)

accepts :: (Eq q) => DFA a q -> [a] -> Bool
accepts m xs = consume m (start m) xs `elem` accepting m
