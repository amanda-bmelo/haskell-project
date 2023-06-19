data Frame = Frame {
  states :: [Char], 
  relations :: [[Char]]
}


-- Função realizada por Gyselle
addProblem :: IO (Frame, String)
addProblem = do
  putStrLn "Deseja adicionar um problema? (S/N)"
  answer <- getLine
  if answer == "S" || answer == "s" then do
    putStrLn "Digite os estados juntos, sabendo que cada estado é um caracter:"
    statesInput <- getLine
    let new_states = statesInput

    putStrLn "Digite as relações no formato 'estado1 rotulo estado2':"
    relationsInput <- getLine
    let new_relations = readRelations relationsInput

    putStrLn "Digite o , sabendo que P representa o programa:"
    programInput <- getLine

    return (Frame new_states new_relations, programInput)
  else
    return (defaultFrame, defaultProgram)
  where
    readRelations :: String -> [[Char]]
    readRelations input = read $ "[" ++ input ++ "]"

    defaultFrame :: Frame
    defaultFrame = Frame ['1', '2'] [['1', 'A', '2'], ['1', 'B', '1'], ['2', 'A', '2']]

    defaultProgram :: String
    defaultProgram = "v(*(;(A,B)),Y)" -- Inválido

-- Função realizada por Gyselle
main :: IO ()
main = do
  (frame, input) <- addProblem
  putStr "\n--- Estados do Frame ---\n";
  print (states frame);
  putStr "\n--- Relacoes do Frame ---\n";
  print (relations frame);
  putStr ("\n--- Programa ---\n"++input++"\n\n");
  print (isValidFrame frame input);
  return ();

-- Função realizada por Amanda
isValidFrame :: Frame -> String -> String
isValidFrame frame program
  | (head (last result)) == "!" = last (take 2 (last result))
  | otherwise =  "Frame valido."
  where result = checkFrame frame program 0 [[['0', '0', head(states frame)]],[]]

-- Função realizada por Amanda
checkFrame :: Frame -> String -> Int -> [[String]] -> [[String]]
checkFrame frame program index path
  | program == "" = path
  | x == ';' = checkSequence frame xs (index+1) path
  | x == 'v' = checkOr frame xs (index+1) path
  | x == '*' = checkIterator frame xs (index+1) path
  | x == '?' = checkExists frame xs (index+1) path
  | otherwise = checkAtom frame program index path
  where 
    x = head program
    xs = tail program

-- Função realizada por Gyselle
checkAtom :: Frame -> String -> Int -> [[String]] -> [[String]]
checkAtom frame program index path
  | not (null new_relation) = checkFrame frame xs (index+1) [([new_relation]++history), (["Frame valido"]++results)]
  | otherwise = checkFrame frame xs (index+1) [history, (["!",("Incompatibilidade no estado "++(show last_state)++", no index "++(show index))]++results)]
  where
    x = head program
    xs = tail program
    history = head path
    results = last path
    relation = head history
    last_state = last relation
    new_relation = findPath (relations frame) [last_state, x]

-- Função realizada por Amanda
checkSequence :: Frame -> String -> Int -> [[String]] -> [[String]]
checkSequence frame program index path = path3 
  where
    xs = tail program
    end = findEndBlock xs 1 1
    middle = findMiddleBlock xs 1 1
    listSeq = listBlock xs (middle-2) (end-2)
    fst_part = head listSeq
    scd_part = last listSeq
    path1 = 
      if  (head fst_part) == '*' then 
        checkFrame frame (take (end-1) xs) (index+1) path -- Fazer um print para checar
      else
        checkFrame frame fst_part (index+1) path
    path2 = 
      if (head (last path1)) /= "!" then 
        if  (head scd_part) == '*' then 
          checkFrame frame (take (end-1) xs) (index+middle) path1 -- Fazer um print para checar
        else
          checkFrame frame scd_part (index+middle) path1
      else 
        path1
    path3 = checkFrame frame (drop (end-1) xs) (end+index) path2
  
-- Função realizada por Amanda
checkOr :: Frame -> String -> Int -> [[String]] -> [[String]]
checkOr frame program index path = path3 
  where
    (_:xs) = program
    end = findEndBlock xs 1 1
    middle = findMiddleBlock xs 1 1
    listSeq = listBlock xs (middle-2) (end-2)
    fst_part = head listSeq
    scd_part = last listSeq
    path1 = 
      if  (head fst_part) == '*' then 
        checkFrame frame (take (end-1) xs) (index+1) path -- Fazer um print para checar
      else
        checkFrame frame fst_part (index+1) path
    path2 = 
      if (head (last path1)) == "!" then 
        if  (head scd_part) == '*' then 
          checkFrame frame (take (end-1) xs) (index+middle) path -- Fazer um print para checar
        else
          checkFrame frame scd_part (index+middle) path
      else path1
    path3 = checkFrame frame (drop (end-1) xs) (end+index) path2

-- Função realizada por Amanda
checkIterator :: Frame -> String -> Int -> [[String]] -> [[String]]
checkIterator frame program index path
  | (head (last path1)) /= "!" = [history,(["Frame valido"]++results)]
  | otherwise = tryPath frame block (index+1) path1 last_relation
  where
    xs = tail program
    end = findEndBlock xs 1 1
    block = take (end-2) xs
    path1 = checkFrame frame block (index+1) path
    history = head path1
    results = last path1
    last_relation = head (head path)
      
-- Função realizada por Gyselle
checkExists :: Frame -> String -> Int -> [[String]] -> [[String]]
checkExists _ _ _ path = [history, (["Frame valido"]++results)]
  where
    history = head path 
    results = last path

-- Funções auxiliares
-- Função realizada por Gyselle
findPath :: [[Char]] -> [Char] -> [Char]
findPath list target 
  | list == [] = []
  | current == last_state && trigger == label = [last_state, label, next]
  | otherwise = findPath xs target 
  where
    relation = head list
    xs = tail list
    current = head relation
    trigger = last (take 2 relation)
    next = last relation
    last_state = head target
    label = last target

-- Função realizada por Amanda
tryPath :: Frame -> String -> Int -> [[String]] -> [Char] -> [[String]]
tryPath frame program index path last_relation
  | not (any(\relation -> (head relation)==(last last_relation)) (relations frame)) = [history, (["!",("Incompatibilidade no estado "++(show (last last_relation))++", no index "++(show index))]++results)]
  | (head new_results) /= "!" = [new_history, (["Frame valido"]++new_results)]
  | (head new_history) == last_relation = [history, (["!",("Incompatibilidade no estado "++(show (last last_relation))++", no index "++(show index))]++results)]
  | otherwise = tryPath frame program index path last_relation
  where
    history = head path
    results = last path
    new_frame = deleteRelation frame (head history)
    new_try = checkFrame new_frame program index [(drop 1 history), (drop 2 results)]
    new_history = head new_try
    new_results = last new_try

-- Função realizada por Amanda
deleteRelation :: Frame -> [Char] -> Frame
deleteRelation frame relation
  | index /= -1 = Frame (states frame) new_relations
  | otherwise = frame
  where 
    index = findRelation relation (relations frame) 0
    start =  take index (relations frame)
    end = drop (index+1) (relations frame)
    new_relations = start++end

-- Função realizada por Amanda
findRelation :: [Char] -> [[Char]] -> Int -> Int
findRelation relation list index
  | list == [] = -1
  | relation == x = index
  | otherwise = findRelation relation xs (index+1)
  where 
    x = head list
    xs = tail list

-- Função realizada por Amanda
findEndBlock :: String -> Int -> Int -> Int
findEndBlock program opened end
  | opened == 0 = end
  | x == '(' = findEndBlock xs (opened+1) (end+1)
  | x == ')' = findEndBlock xs (opened-1) (end+1)
  | otherwise = findEndBlock xs opened (end+1)
  where
    x = head program
    xs = tail program

-- Função realizada por Amanda
findMiddleBlock :: String -> Int -> Int -> Int
findMiddleBlock program comma end
  | comma == 0 = end
  | x == 'v' || x == ';' = findMiddleBlock xs (comma+1) (end+1)
  | x == ',' = findMiddleBlock xs (comma-1) (end+1)
  | otherwise = findMiddleBlock xs comma (end+1)
  where
    x = head program
    xs = tail program

-- Função realizada por Amanda
listBlock :: String -> Int -> Int  -> [String]
listBlock block middleBlock endBlock = [take middleBlock block, drop (middleBlock+1) (take endBlock block)]
  

