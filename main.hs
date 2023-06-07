type Estado = String
type Rotulo = String
type Aresta = (Estado, Rotulo, Estado) -- 1 --b2--> 2
type Frame = ([Estado], [Aresta])

data Programa
  = Atom Programa -- alpha
  | Sequence Programa -- alpha;b1
  | Or Programa Programa -- alpha; 1? v ~1?
  | Iterator Programa -- alpha; (1?;b1)*
  | NotExists Programa -- alpha;~1?
  | Exists Programa -- alpha;1?
  deriving (Show)
-- Pergunta para Prof: And, Box, Diamond: precisam?

data No = No{ -- Árvore
    programa :: [Programa], -- Lista de Programas
    esquerdo :: No,
    direito :: No
}

-- Main de exemplo:
-- Elevador de 2 andares que somente sobe
main :: IO ()
main = do
  let frame = (["1", "2"], [("1", "b2", "2"), ("1", "b1", "1"), ("2", "b2", "2")]);
  let input = ";(P)v(;(?(b1))(;(?(1))P), v((;(?(b2))(;(?(1));(P)P)),(;(?(2))P)))";
  print frame;
  print input;
  return ();
  -- P;(b1?;(1?;P))v(b2?;(1?;P;P)v(2?;P))


--TODO Gyselle - Alterar a main para aceitar input:
--1. Perguntar se a pessoa quer colocar um problema (S/N)
--1.1. Caso a pessoa não queira rodar um programa, rodar o exemplo nosso.
--1.2. Caso sim, perguntar os estados, as relações, e o programa.

--TODO Gyselle - Parser:
--1. Transformar String de input em uma árvore de programas
--1.1. A cada v deve ter uma bifurcação (isso OU aquilo)
--1.2. A cada * deve ter uma bifurcação (se executar 0 vezes ou 1/mais vezes)

--TODO Amanda - Verificar programa:
--1. Ver se o frame é válido para o programa em questão.
--1.1. Se o programa é atómico, o frame é válido.
--1.2. Se não, checar a árvore.
--1.2.1. Cada parte de programa sequencial precisa ser possível no frame, inclusive as bifurcações.

-- TODO Amanda - Reporte incompatibilidade do frame:
--1. Caso em uma parte da árvore o frame esteja inválido:
--1.1. Reportar essa parte e interromper a execução do programa.
--1.2. O reporte deve conter a relação em que é inválido, e a parte do programa correspondente.