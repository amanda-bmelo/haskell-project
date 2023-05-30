import Data.List (nub)

type Estado = String
type Rotulo = String
type Aresta = (Estado, Rotulo, Estado)
type Frame = ([Estado], [Aresta])

type Programa = [Rotulo]

checkFrameValidade :: Frame -> Programa -> Either String ()
checkFrameValidade (estados, arestas) programa = do
  checkEstados estados
  checkArestas arestas
  checkEstadosFaltando estados arestas
  checkPrograma programa arestas

checkEstados :: [Estado] -> Either String ()
checkEstados estados =
  if length estados /= length (nub estados)
    then Left "O conjunto de estados contém estados duplicados."
    else Right ()

checkArestas :: [Aresta] -> Either String ()
checkArestas arestas =
  if length arestas /= length (nub arestas)
    then Left "O conjunto de arestas contém arestas duplicadas."
    else Right ()

checkPrograma :: Programa -> [Aresta] -> Either String ()
checkPrograma programa arestas = do
  case filter (\(_, rotulo, _) -> rotulo `notElem` programa) arestas of
    [] -> Right ()
    arestasInvalidas -> Left $ "As seguintes arestas não estão presentes no programa: " ++ show arestasInvalidas

checkEstadosFaltando :: [Estado] -> [Aresta] -> Either String ()
checkEstadosFaltando estados arestas = do
  case filter (\(origem, _, destino) -> origem `notElem` estados || destino `notElem` estados) arestas of
    [] -> Right ()
    estadosFaltando -> Left $ "As seguintes arestas contêm estados não presentes na lista de estados: " ++ show estadosFaltando


-- Exemplo básico
main :: IO ()
main = do
  let frame = (["A", "B", "C"], [("A", "passo1", "B"), ("B", "passo2", "C")])
      programa = ["passo1", "passo2"]
  case checkFrameValidade frame programa of
    Right _ -> putStrLn "O frame é válido para o programa."
    Left errMsg -> putStrLn $ "O frame não é válido para o programa. Erro: " ++ errMsg
