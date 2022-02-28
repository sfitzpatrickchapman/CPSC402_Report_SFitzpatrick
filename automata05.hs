-- automata05.hs
-- convert NFAs to DFAs
-- exercise: complete nfa2dfa

data DFA s = DFA {
  dfa_initial :: s,
  dfa_final :: s -> Bool,
  dfa_delta :: s -> Char -> s }

data NFA s = NFA {
  nfa_initial :: s,
  nfa_final :: s -> Bool,
  nfa_delta :: s -> Char -> [s] }

-- an NFA for all words that have at least two 'a' and end on 'a'
data Automaton1 = Q0 | Q1 | Q2
  deriving (Show,Eq)

nfa1 = NFA {
  nfa_initial = Q0,
  nfa_final = let
    final Q0 =  False
    final Q1 = False
    final Q2 = True
    in final,
  nfa_delta  = let
    delta Q0 'a' =  [Q0, Q1]
    delta Q0 _ =  [Q0]
    delta Q1 'a' =  [Q1,Q2]
    delta Q1 _ =  [Q1]
    delta Q2 _ =  []
    in delta }

-- run a DFA on a state and a word
dfa_run :: DFA s -> s -> [Char] -> s
dfa_run dfa q [] = q
dfa_run dfa q (c:cs) = dfa_run dfa (dfa_delta dfa q c) cs

-- run an NFA on a state and a word
nfa_run :: NFA s -> s -> [Char] -> [s]
nfa_run nfa q [] = return q
nfa_run nfa q (c:cs) = do
  next <- nfa_delta nfa q c
  nfa_run nfa next cs

-- for a DFA and a state return its language
dfa_semantics :: DFA s -> s -> [Char] -> Bool
dfa_semantics dfa q w = dfa_final dfa (dfa_run dfa q w)

-- take the disjunction of a list of Booleans
disjunction :: [Bool] -> Bool
disjunction [] = False
disjunction (p:ps) = p || (disjunction ps)

-- for an NFA and a state return its language
nfa_semantics :: NFA s -> s -> [Char] -> Bool
nfa_semantics nfa q w = disjunction (map (nfa_final nfa) (nfa_run nfa q w))

-- for a DFA check whether a word is in the language
dfa_recognize :: DFA s -> [Char] -> Bool
dfa_recognize dfa word = dfa_semantics dfa (dfa_initial dfa) word

-- for an NFA check whether a word is in the language
nfa_recognize :: NFA s -> [Char] -> Bool
nfa_recognize nfa word = nfa_semantics nfa (nfa_initial nfa) word

- convert an NFA to a DFA
nfa2dfa :: NFA s -> DFA [s]
nfa2dfa nfa = DFA {
-- exercise: correct the next three definitions
dfa_initial = [nfa_initial nfa],
dfa_final = let final qs = disjunction (map(nfa_final nfa) qs) in final,
dfa_delta = let
  f [] c = []
  f (q:qs) c = concat [nfa_delta nfa q c, f qs c] in f
}

-- for testing
-- from https://stackoverflow.com/a/16108856/4600290
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldl (\seen x -> if x `elem` seen
                                     then seen
                                     else seen ++ [x]) []

-- both NFA and DFA should yield the same results
-- if nfa2dfa is implemented correctly

main = do
  -- see above for the definition of the NFA nfa1
  let word = "bbbg" -- change for testing
  let dfa1 = nfa2dfa nfa1 -- the determinization of nfa1
  putStrLn "NFA:"
  print $ nfa_recognize nfa1 word -- does nfa1 accept word?
  print $ removeDuplicates $ nfa_run nfa1 (nfa_initial nfa1) word
  putStrLn " "
  putStrLn "DFA:"
  print $ dfa_recognize dfa1 word -- does dfa1 accept word?
  print $ removeDuplicates $ dfa_run dfa1 (dfa_initial dfa1) word
