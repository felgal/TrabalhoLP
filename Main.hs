module Main where

import           Control.Monad
import           Data.Bool
import           Data.Char
import           Data.Foldable
import           Data.Graph
import           Data.List.Split
import           Debug.Trace

(?) :: (Ord a) => a -> [a] -> Bool
(?) _ [] = False
(?) x y
  | x == head y = True
  | otherwise = (?) x (tail y)

vertices' :: Graph -> (Vertex, Vertex)
vertices' grafo = (minimum $ vertices grafo, maximum $ vertices grafo)

uniao :: Graph -> Graph -> Graph
uniao grafo1 grafo2 =
    let vert = vertices' grafo1
        arestas1 = edges grafo1
        arestas2 = edges grafo2
        arestas = arestas1 ++ arestas2
    in  buildG vert arestas

achaDivisao :: String -> Int -> Int
achaDivisao pdl quantEnt =
  let
    topoString = let
      in if (length pdl)>0
        then
          (head pdl)
        else
          '/'
    in if topoString == '('
      then
        ((achaDivisao (tail pdl) (quantEnt+1)  )+ 1)
      else
        if topoString == ')'
          then
            ((achaDivisao (tail pdl) (quantEnt-1)  )+ 1)
        else
          if quantEnt==0
            then  0
          else
            ((achaDivisao (tail pdl) (quantEnt) ) + 1)

substituiCharInPDL :: String->Char->Int->String
substituiCharInPDL pdl cTroca valTroca =
  let
    valTrocaChar = show valTroca
    valTrocaCharF = intToDigit valTroca
    c = let
      in if (length pdl)>0
        then
          (head pdl)
        else
          '/'
    in if c=='/'
      then []
      else if c==cTroca
        then ([valTrocaCharF]++ (substituiCharInPDL (tail pdl) cTroca valTroca))
        else ([c]++ (substituiCharInPDL (tail pdl) cTroca valTroca))

substituiPDL :: String->Int->Int->String
substituiPDL pdl quantAtual quantTotal =
  let
    c = let
      in if (length pdl)>0
        then
          (head pdl)
        else
          '/'
    in if c=='/'
      then []
      else if (not (c=='>')) && (not (c=='-')) && (not (c=='&')) && (not (c=='|')) && (not (c=='(')) && (not (c==')')) && (not (c=='0')) && (not (c=='1')) && (not (c=='2')) && (not (c=='3')) && (not (c=='4')) && (not (c=='5')) && (not (c=='6')) && (not (c=='7')) && (not (c=='8')) && (not (c=='9'))
        then ((substituiPDL (substituiCharInPDL pdl c (quantTotal-quantAtual)) (quantAtual-1) quantTotal))
        else ([c]++(substituiPDL (tail pdl) quantAtual quantTotal))

verificaPDLcomEntrada :: String -> Bool
verificaPDLcomEntrada pdl =
  let
    posDiv = achaDivisao pdl 0
    in if posDiv==0
      then
        True
      else let
        conector = pdl !! posDiv
        parte1 = take (posDiv-2) (drop 1 pdl)
        parte2 = drop (posDiv+2) (take 1 pdl)
        in if conector=='&'
          then ((verificaPDLcomEntrada parte1) && (verificaPDLcomEntrada parte2))
          else if conector=='|'
            then ((verificaPDLcomEntrada parte1) || (verificaPDLcomEntrada parte2))
            else if conector=='>'
              then ((not(verificaPDLcomEntrada parte1)) || (verificaPDLcomEntrada parte2))
              else if conector=='-'
                then ((verificaPDLcomEntrada parte1) && (verificaPDLcomEntrada parte2) || (not(verificaPDLcomEntrada parte1)) && (not(verificaPDLcomEntrada parte2)) )
                else False


geraConcatDeInts :: [String] ->[[Int]]
geraConcatDeInts str =
  let
    c = let
      in if (length str)>0
        then
          (head str)
        else
          "/"
  in if c=="/"
    then
      []
    else
      [(map read (splitOn "," c) :: [Int])] ++ (geraConcatDeInts (tail str))

main = do
  --Entrada da quantidade de nós
  --entNos<- getLine
  --let quantNos = (read entNos :: Int)

  --Entrada das areas no formato 1/2,3;2/1,3;3/1
  entArestas<- getLine
  tuplasNos <- return $ splitOn ";" entArestas

  --verifica o tamanho da entrada de aresta e cria a lista de criação do grafo
  let tamTuplas = (length tuplasNos :: Int)
  let nosSemi = forM [0..tamTuplas-1] $ \a -> do
        tuplaAtual <- return $ tuplasNos !! a
        noAtual <- return $ splitOn "/" tuplaAtual
        valNoAtual <- return $ head noAtual
        vizinhosAtuais <- return $ noAtual !! 1
        vizinhos <- return $ splitOn "," vizinhosAtuais
        return $ (valNoAtual,valNoAtual, vizinhos) :: ([Char],([Char],[Char],[[Char]]))

  nos <- return $ snd nosSemi
  let (graph, vertexMap) = graphFromEdges' nos
  print $ edges graph

  --entra com a quantidade de variaveis
  --entVar<- getLine
  --let quantVar = (read entVar :: Int)

  --cria a lista de valores verdadeiros a partir da entrada,exemplo 0,1;0,1,2
  entVars<- getLine
  tuplasVars <- return $ splitOn ";" entVars
  varsSemi <- return $ geraConcatDeInts tuplasVars
  print varsSemi

  --Entra com o pdl no formato ((q)&(p))-(p)
  entPdl <- getLine
  print $ verificaPDLcomEntrada entPdl

  --realiza a substituição de caracteres
  novoPdl <- return $ substituiPDL entPdl (length tuplasVars) (length tuplasVars)
  print $ novoPdl
