import Text.Parsec

type Programa = String
type Estado = String
type Rotulo = String
type Aresta = (Estado, Rotulo, Estado)
type Frame = ([Estado], [Aresta])

type Parser a = Parsec String () a

data Formula
  = Atom Rotulo Estado
  | Neg Formula
  | And Formula Formula
  | Or Formula Formula
  | Exists Rotulo Estado Formula
  | Program Programa
  deriving (Show)

main :: IO ()
main = do
  let frame = (["1", "2"], [("1", "b2", "2"), ("1", "b1", "1"), ("2", "b2", "2"), ("2", "b1", "1")])
      programa = "r"
      formula = "[]r v(?b1 ;1)(?b2 ;2)"
  case parse formulaParser "" formula of
    Left err -> putStrLn ("Erro de análise sintática: " ++ show err)
    Right parsedFormula -> putStrLn (verificarFormula frame programa parsedFormula)

parseFormula :: String -> Either String Formula
parseFormula input = case parse formulaParser "" input of
  Left err -> Left (show err)
  Right formula -> Right formula

formulaParser :: Parser Formula
formulaParser = spaces *> (atomParser <|> negParser <|> andParser <|> orParser <|> existsParser <|> programParser) <* spaces

atomParser :: Parser Formula
atomParser = Atom <$> rotuloParser <*> estadoParser

negParser :: Parser Formula
negParser = Neg <$ char '~' <*> formulaParser

andParser :: Parser Formula
andParser = And <$ char '^' <*> formulaParser <*> formulaParser

orParser :: Parser Formula
orParser = Or <$ char 'v' <*> formulaParser <*> formulaParser

existsParser :: Parser Formula
existsParser = Exists <$ string "(?" <*> rotuloParser <* char ';' <*> estadoParser <* char ')' <*> formulaParser

programParser :: Parser Formula
programParser = Program <$> many1 letter

rotuloParser :: Parser Rotulo
rotuloParser = many1 letter

estadoParser :: Parser Estado
estadoParser = many1 digit

verificarFormula :: Frame -> Programa -> Formula -> String
verificarFormula frame programa formula = case evalFormula frame programa formula of
  Left err -> err
  Right True -> "Essa fórmula é válida nesse frame."
  Right False -> "Essa fórmula não é válida nesse frame, ponto de incompatibilidade: " ++ getIncompatibilityPoint frame formula

evalFormula :: Frame -> Programa -> Formula -> Either String Bool
evalFormula frame programa (Atom r e) = Right (relacaoValida frame (e, r, e))
evalFormula frame programa (Neg f) = case evalFormula frame programa f of
  Right True -> Right False
  Right False -> Right True
  Left err -> Left err
evalFormula frame programa (And f1 f2) = case (evalFormula frame programa f1, evalFormula frame programa f2) of
  (Right True, Right True) -> Right True
  (Right _, Right _) -> Right False
  (Left err, _) -> Left err
  (_, Left err) -> Left err
evalFormula frame programa (Or f1 f2) = case (evalFormula frame programa f1, evalFormula frame programa f2) of
  (Right False, Right False) -> Right False
  (Right _, Right _) -> Right True
  (Left err, _) -> Left err
  (_, Left err) -> Left err
evalFormula frame programa (Exists r x f) = case lookup r env of
  Just e -> evalFormula frame programa f
  Nothing -> Left ("Rótulo não encontrado no ambiente: " ++ r)
  where
    env = [(x, e) | (_, e, _) <- snd frame]
evalFormula frame programa (Program p) = case programa of
  [p'] | programa == [p'] -> Right True
  _ -> Right False

relacaoValida :: Frame -> Aresta -> Bool
relacaoValida (estados, arestas) (origem, rotulo, destino) = elem origem estados && elem destino estados && elem (origem, rotulo, destino) arestas

getIncompatibilityPoint :: Frame -> Formula -> String
getIncompatibilityPoint _ (Atom _ _) = "na fórmula atômica"
getIncompatibilityPoint frame (Neg f) = "na negação da fórmula: " ++ getIncompatibilityPoint frame f
getIncompatibilityPoint frame (And f1 f2) = "na conjunção das fórmulas: " ++ getIncompatibilityPoint frame f1 ++ " e " ++ getIncompatibilityPoint frame f2
getIncompatibilityPoint frame (Or f1 f2) = "na disjunção das fórmulas: " ++ getIncompatibilityPoint frame f1 ++ " e " ++ getIncompatibilityPoint frame f2
getIncompatibilityPoint frame (Exists r x f) = "na fórmula com rótulo " ++ r ++ " e variável " ++ x ++ ": " ++ getIncompatibilityPoint frame f
getIncompatibilityPoint _ (Program _) = "no programa"
