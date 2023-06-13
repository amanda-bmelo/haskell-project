type Gatilho = String
type Estado = String
type Rotulo = String
type Aresta = (Estado, Rotulo, Estado) -- 1 --b2--> 2
type Frame = ([Estado], [Aresta])

operadores = [";", "*", "~", "v", "?"]
-- Pergunta para Prof: And, Box, Diamond: precisam?
data Tree = Tree {
  node :: [String],
  nodes :: [Tree]
}

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

    return ((estados, relacoes), programaInput)
  else
    return (defaultFrame, defaultProgram)
  where
    lerRelacoes :: String -> [Aresta]
    lerRelacoes input = read $ "[" ++ input ++ "]"

    defaultFrame :: Frame
    defaultFrame = (["1", "2"], [("1", "b2", "2"), ("1", "b1", "1"), ("2", "b2", "2")])

    defaultProgram :: String
    defaultProgram = ";(P)v(;(?(b1))(;(?(1))P),v((;(?(b2))(;(?(1));(P)P)),(;(?(2))P)))" -- P; (b1?;1?;P) v (b2?;(1?;P;P)v(2?;P)))


-- main :: IO ()
-- main = do
--   (frame, input) <- adicionarProblema
--   print frame;
--   print input;
--   return ();
  
-- Main de exemplo:
-- Elevador de 2 andares que somente sobe
main :: IO ()
main = do
  -- (1) --b1--> (1) --b2--> (2) --b2--> (2)
  let frame = (["1", "2", "3"], [("1", "b2", "2"), ("1", "b1", "1"), ("2", "b2", "2")]);
      trigger = "P";
      tree = Tree ["P"] [ Tree ["b1","?","1","?"] [], Tree ["b2", "?", "1", "?", "2", "?"] []]; -- Usar só strings mesmo

  return ();

--TODO Gyselle - Parser:
--1. Transformar String de input em uma árvore de programas
--1.1. A cada v deve ter uma bifurcação (isso OU aquilo)
--1.2. A cada * deve ter uma bifurcação (se executar 0 vezes ou 1/mais vezes)
-- Ex: (1?;b1)* 2?
-- No: 1? --> No: b1 -->: No: 2?
-- No: 2?

--TODO Amanda - Verificar programa:
--1. Ver se o frame é válido para o programa em questão.
--1.1. Se o programa é atómico, o frame é válido.
--1.2. Se não, checar a árvore.
--1.2.1. Cada parte de programa sequencial precisa ser possível no frame, inclusive as bifurcações.

--TODO Amanda - Reporte incompatibilidade do frame:
--1. Caso em uma parte da árvore o frame esteja inválido:
--1.1. Reportar essa parte e interromper a execução do programa.
--1.2. O reporte deve conter a relação em que é inválido, e a parte do programa correspondente.