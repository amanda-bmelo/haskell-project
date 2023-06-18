data Frame = Frame {
  states :: [String], 
  relations :: [(String, String, String)]
}


-- Função realizada por Gyselle
adicionarProblema :: IO (Frame, String)
adicionarProblema = do
  putStrLn "Deseja adicionar um problema? (S/N)"
  resposta <- getLine
  if resposta == "S" || resposta == "s" then do
    putStrLn "Digite os estados separados por espaço:"
    estadosInput <- getLine
    let estados = words estadosInput

    putStrLn "Digite as relações no formato 'estado1 rotulo estado2':"
    relacoesInput <- getLine
    let relacoes = lerRelacoes relacoesInput

    putStrLn "Digite o programa, sabendo que P representa o programa:"
    programaInput <- getLine

    return (Frame estados relacoes, programaInput)
  else
    return (defaultFrame, defaultProgram)
  where
    lerRelacoes :: String -> [(String, String, String)]
    lerRelacoes input = read $ "[" ++ input ++ "]"

    defaultFrame :: Frame
    defaultFrame = Frame ["1", "2"] [("1", "A", "2"), ("1", "B", "1"), ("2", "A", "2")]

    defaultProgram :: String
    defaultProgram = "v(;(*(A),B),A)" -- (A* ; B) v A || T=14

-- Função realizada por Gyselle
main :: IO ()
main = do
  (frame, input) <- adicionarProblema
  print (head (states frame));
  print input;
  return ();

isValidFrame :: Frame -> String -> String
isValidFrame frame programa
  | x == "!" = head result
  | otherwise =  "Frame válido."
  where (x:result) = checkFrame frame programa 0 (take 1 (states frame)) []

checkFrame :: Frame -> String -> Int -> [String] -> [String] -> [String]
checkFrame _ "" _ _ results = results
checkFrame frame (x:xs) index history results
  | x == ';' = checkSequence frame xs (index+1) history results
  | x == 'v' = checkOr frame xs (index+1) history results
  | x == '*' = checkIterator frame xs (index+1) history results
  | x == '?' = checkExists frame xs (index+1) history results
  | otherwise = checkAtom frame xs (index+1) history results

checkAtom :: Frame -> String -> Int -> [String] -> [String] -> [String]
checkAtom frame (x:xs) index (last_state:history) results
  | any (\(current, alpha, next) -> (alpha == [x] && current == last_state)) (relations frame) = checkFrame frame xs (index+1) (next:history) ("Frame válido":results)
  | otherwise = checkFrame frame xs (index+1) history (["!",("Incompatibilidade no estado "++last_state++", no index "++(show index))]++results)

checkSequence :: Frame -> String -> Int -> [String] -> [String] -> [String]
checkSequence frame (_:xs) index history results = result3 
  where
    end = findEndBlock xs 1 (index+1)
    middle = findMiddleBlock xs 1 1
    listSeq = listBlock xs (middle-2) (end-index-2)
    result1 = checkFrame frame (head listSeq) (index+1) history results
    result2 = if head result1 /= "!" then checkFrame frame (last listSeq) (index+middle) history results else result1
    result3 = checkFrame frame (drop (length xs - end) xs) end history result2
  

checkOr :: Frame -> String -> Int -> [String] -> [String] -> [String]
checkOr frame (_:xs) index history results = result3 
  where
    end = findEndBlock xs 1 (index+1)
    middle = findMiddleBlock xs 1 1
    listSeq = listBlock xs (middle-2) (end-index-2)
    result1 = checkFrame frame (head listSeq) (index+1) history results
    result2 = if head result1 == "!" then checkFrame frame (last listSeq) (index+middle) history results else result1
    result3 = checkFrame frame (drop (length xs - end) xs) end history result2

checkIterator :: Frame -> String -> Int -> [String] -> [String] -> [String]
checkIterator frame (_:xs) index (last_state:history) results = result3 
  where
    end = findEndBlock xs 1 (index+1)
    atom = take (end-1) xs
    valid_states = checkStates frame atom [last_state] [] results
    result0 = ("Frame válido":reuslts)
    result1 = if lenght valid_states == 1 && any (\(last_state, atom, next) -> next == last_state) (relations frame) then
        ("Frame válido":results)
        else
          (["!",("Incompatibilidade no estado "++last_state++", no index "++(show index))]++results)
        
    result2 = if head result1 == "!" then checkFrame frame atom (index+1) history results1 else result1
    result3 = checkFrame frame (drop (length xs - end) xs) end history result2

checkStates ::  Frame -> String -> [String] -> [String] -> [String]
checkIterator frame atom (last_state:history) relations_history results
  | all (\state -> state `elem` states frame) (last_state:history) = (last_state:history)
  | all (\(c,atom,n) -> (c,atom,n) `elem` relations_history) relations frame = (last_state:history)
  | any (\(last_state, atom, next) -> ( next /= last_state && not ((last_state, atom, next) `elem` relations_history))) (relations frame) = checkStates frame atom (next:(last_state:history)) ((last_state, atom, next):relations_history) ("Frame válido":results)
  | null history = (history++[last_state])
  | otherwise = checkStates frame atom (head history:(history++[last_state])) relations_history results

checkExists :: Frame -> String -> Int -> [String] -> [String] -> [String]
checkExists _ "" _ _ results = results
checkExists frame (x:xs) index history results =
  if x == '?'
    then
      let result1 = checkFrame frame xs (index+1) history results
          result2 = if head result1 /= "!" then checkFrame frame (drop 1 xs) (index+1) history results else result1
      in checkExists frame (dropWhile (/= ';') xs) (index+1) history result2
    else
      checkSequence frame (x:xs) index history results

findEndBlock :: String -> Int -> Int -> Int
findEndBlock _ 0 end = end
findEndBlock (x:program) opened end
  | x == '(' = findEndBlock program (opened+1) (end+1)
  | x == ')' = findEndBlock program (opened-1) (end+1)
  | otherwise = findEndBlock program opened (end+1)

findMiddleBlock :: String -> Int -> Int -> Int
findMiddleBlock _ 0 end = end
findMiddleBlock (x:program) comma end
  | x == 'v' = findMiddleBlock program (comma+1) (end+1)
  | x == ';' = findMiddleBlock program (comma+1) (end+1)
  | x == ',' = findMiddleBlock program (comma-1) (end+1)
  | otherwise = findMiddleBlock program comma (end+1)

listBlock :: String -> Int -> Int -> [String]
listBlock block endBlock middleBlock = [take middleBlock block, drop (endBlock-middleBlock-1) block]
  

