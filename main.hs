import Data.List (nub)

type State = String
type Label = String
type Edge = (State, Label, State)
type Frame = ([State], [Edge])

type Program = [Label]

checkFrameValidity :: Frame -> Program -> Either String ()
checkFrameValidity (states, edges) program = do
  checkStates states
  checkEdges edges
  checkProgram program edges

checkStates :: [State] -> Either String ()
checkStates states =
  if length states /= length (nub states)
    then Left "O conjunto de estados contém elementos duplicados."
    else Right ()

checkEdges :: [Edge] -> Either String ()
checkEdges edges =
  if length edges /= length (nub edges)
    then Left "O conjunto de arestas contém elementos duplicados."
    else Right ()

checkProgram :: Program -> [Edge] -> Either String ()
checkProgram program edges =
  case filter (\(from, label, to) -> label `notElem` program) edges of
    [] -> Right ()
    invalidEdges -> Left $ "As seguintes arestas não estão presentes no programa: " ++ show invalidEdges

-- Exemplo básico
main :: IO ()
main = do
  let frame = (["A", "B", "C"], [("A", "label1", "B"), ("B", "label2", "C")])
      program = ["label1", "label2"]
  case checkFrameValidity frame program of
    Right _ -> putStrLn "O frame é válido para o programa."
    Left errMsg -> putStrLn $ "O frame não é válido para o programa. Erro: " ++ errMsg
