# Projeto de Haskell

## Descrição
Implementar um programa em Haskell que receba como entrada um frame PDL (i.e. um grafo com arestas rotuladas) F = (W, Rα) tal que W é um conjunto de estados (vértices) e Rα é uma relação binária indexada (arestas rotuladas) e um programa PDL π. A saída deverá ser o resultado da verificação de se F corresponde a um frame válido para π, isto é, se F é um grafo induzido por π. Caso a resposta seja negativa, uma mensagem de erro deve explicitar o ponto de incompatibilidade.


### Compilação
Para gerar o arquivo:
`ghc --make main`

Para rodar a compilação:
`./main`

### Relatório
Distribuição das funções
| Amanda                | Gyselle      |
|-----------------------|--------------|
| checkSequence         | addProblem   |
| checkOr               | main         |
| checkIterator         | isValidFrame |
| checkIteratorSequence | checkFrame   |
| tryPath               | checkAtom    |
| deleteRelation        | findPath     |
| findRelation          | checkExists  |
| findEndBlock          |              |
| findMiddleBlock       |              |
| listBlock             |              |