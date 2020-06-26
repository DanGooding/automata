
import DFA
import qualified NFAe
import RE
import Text.Printf (printf)

-- a DFA accepting strings with three consecutive a's
-- (over the alphabet "ab")
m_AAA :: DFA Char Int
m_AAA = DFA
  -- state is the current number of consecutive a's
  { states = [0..3]
  , transition = \ q x ->
      case (q, x) of
        (3, _) -> 3
        (n, 'A') -> n + 1
        (_, 'B') -> 0
  , start = 0
  , accepting = [3]
  }

-- a regular expression matching the same set of strings
-- (a|b)*aaa(a|b)*
r_AAA :: RE Char
r_AAA =
  Concat
    (Star
      (Union
        (Sym 'A')
        (Sym 'B')))
    (Concat
      (Sym 'A')
      (Concat
        (Sym 'A')
        (Concat
          (Sym 'A')
          (Star
            (Union
              (Sym 'A')
              (Sym 'B'))))))



-- any string with an 'x' 4th to last
m_B_then_any_3 :: NFAe.NFAe Char Int
m_B_then_any_3 = NFAe.NFAe
  { NFAe.states = [0..4]
  , NFAe.transition = \ q x ->
      case (q, x) of
        (0, 'B') -> [0, 1]
        (0, _) -> [0]
        (4, _) -> []
        (n, _) -> [n + 1]
  , NFAe.epsilonTransition = const []
  , NFAe.start = 0
  , NFAe.accepting = [4]
  }



alternate :: [[a]] -> [a]
alternate ((x:xs):xss) = x : alternate (xss ++ [xs])

strings :: [a] -> [[a]]
strings xs = [] : alternate (map (\ x -> map (++[x]) (strings xs)) xs)

main :: IO ()
main = do
  let alphabet = "AB"

  putStrLn "the DFA `m_AAA` accepts strings with 3 consective A's"
  print $ take 20 $ filter (accepts m_AAA) $ strings alphabet

  putStrLn $ "\nthe regex `r_AAA` = "++ prettyPrint r_AAA ++ " is equivalent"
  print $ take 20 $ filter (matches r_AAA) $ strings alphabet

  putStrLn "matching is done by 'compiling' to a NFAe (Kleene's theorem)"
  print $ take 20 $ filter (NFAe.accepts $ compile r_AAA) $ strings alphabet

  putStrLn "\n\nthe NFA `m_B_then_any_3` accepts strings with 'B' 4th to last"
  print $ take 20 $ filter (NFAe.accepts m_B_then_any_3) $ strings alphabet

  putStrLn "\nlike all NFA-epsilon, it can be converted to a DFA by the powerset construction"
  putStrLn "however this gives an exponential (2^n) increase in the number of states:"
  printf "from %d to %d\n" (length $ NFAe.states m_B_then_any_3) (length $ states $ NFAe.toDFA m_B_then_any_3)
