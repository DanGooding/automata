
module RE
  ( RE (..)
  , prettyPrint
  , compile
  , matches
  , Labelled
  ) where

import NFAe


data RE a = Empty                 -- matches the empty string
          | Null                  -- never matches
          | Sym a                 -- matches a
          | Concat (RE a) (RE a)  -- both in series
          | Union (RE a) (RE a)   -- either
          | Star (RE a)           -- zero or more
  deriving (Show)


prettyPrint :: RE Char -> String
prettyPrint Empty = ""
prettyPrint Null = "<null>"
prettyPrint (Sym x) = [x]
prettyPrint (Concat r s) = prettyPrint r ++ prettyPrint s
prettyPrint (Union r s) = "(" ++ prettyPrint r ++ "|" ++ prettyPrint s ++ ")"
prettyPrint (Star r) = "(" ++ prettyPrint r ++ ")*"
-- don't always need brackets, but sometimes may  (precedence based printing)


-- facilitates disjoint-union of sets of states (to avoid clashes):
-- used to combine two automata (labelling which each state came from)
-- or to add new 'artificial' states
-- could just use `Either q1 q2`, but then the type
-- depends on how many unions were done
data Labelled q = State q
                | Artificial Integer
                | Labelled Integer (Labelled q)
  deriving (Eq, Ord, Show)

-- produce an automaton that accepts the same stings as this regex
compile :: (Eq a) => RE a -> NFAe a (Labelled Integer)

-- match the empty string
compile Empty = NFAe
  { states = [State 0]              -- start in accepting state
  , transition = const $ const []   -- leaves this state for any input
  , epsilonTransition = const []
  , start = State 0
  , accepting = [State 0]
  }

-- never match anything
compile Null = NFAe
  { states = [State 0]
  , transition = const $ const []
  , epsilonTransition = const []
  , start = State 0
  , accepting = []  -- no accepting states
  }

-- match the symbol x
compile (Sym x) = NFAe
  { states = State <$> [0, 1]
  , transition = \ (State i) a ->
      case i of
        0 | a == x -> [State 1]  -- go 0 -> 1 for input x only
        _          -> []
  , epsilonTransition = const []
  , start = State 0
  , accepting = [State 1]
  }

-- match strings where the first part matches r1, and the rest matches r2
compile (Concat r1 r2) = NFAe
  { states = (Labelled 1 <$> states m1) ++ (Labelled 2 <$> states m2)
  , transition = \ (Labelled n q) a ->
      Labelled n <$> transition (case n of 1 -> m1 ; 2 -> m2) q a

  , epsilonTransition = \ (Labelled n q) ->
      case n of
        1 -> (Labelled 1 <$> epsilonTransition m1 q)
              ++ if q `elem` accepting m1
                   then [Labelled 2 $ start m2]  -- after matching r1, the rest must match r2
                   else []
        2 -> Labelled 2 <$> epsilonTransition m2 q

  , start = Labelled 1 $ start m1
  , accepting = Labelled 2 <$> accepting m2  -- end by matching r2
  }
  where
    m1 = compile r1
    m2 = compile r2

-- match anything that matches r1 or r2
compile (Union r1 r2) = NFAe
  -- special artificial start state, with epsilon transitions to the starts of m1 and m2
  { states = [Labelled 0 $ Artificial 0] ++ (Labelled 1 <$> states m1) ++ (Labelled 2 <$> states m2)
  , transition = \ (Labelled n x) a ->
      case (n, x) of
        (0, Artificial 0) -> []
        (1, q) -> Labelled 1 <$> transition m1 q a
        (2, q) -> Labelled 2 <$> transition m2 q a

  , epsilonTransition = \ (Labelled n x) ->
      case (n, x) of
        (0, Artificial 0) -> [Labelled 1 $ start m1, Labelled 2 $ start m2]
        (1, q) -> Labelled 1 <$> epsilonTransition m1 q
        (2, q) -> Labelled 2 <$> epsilonTransition m2 q

  , start = Labelled 0 (Artificial 0)
  , accepting = (Labelled 1 <$> accepting m1) ++ (Labelled 2 <$> accepting m2)
  }
  where
    m1 = compile r1
    m2 = compile r2

-- match anything that can be split into pieces, each of which matches r
-- or the empty string
compile (Star r) = NFAe
  -- artificial state, which is the start and end,
  -- returned to after being accepted by r
  { states = [Labelled 0 (Artificial 0)] ++ (Labelled 1 <$> states m)

  , transition = \ (Labelled n x) a ->
      case (n, x) of
        (0, Artificial 0) -> []
        (1, q) -> Labelled 1 <$> transition m q a

  , epsilonTransition = \ (Labelled n q) ->
      case (n, q) of
        (0, Artificial 0) -> [Labelled 1 $ start m]
        (1, q) -> (Labelled 1 <$> epsilonTransition m q)
                    ++ if q `elem` accepting m
                         then [Labelled 0 $ Artificial 0]
                         else []

  , start = Labelled 0 (Artificial 0)
  , accepting = [Labelled 0 (Artificial 0)]
  }
  where m = compile r


matches :: (Eq a) => RE a -> [a] -> Bool
matches r xs = accepts (compile r) xs
