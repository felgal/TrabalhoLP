module Main where

import           Control.Monad
import           Data.Bool
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

main = do
  --Entrada da quantidade de nós
  entNos<- getLine
  let quantNos = (read entNos :: Int)
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

  --Entra com o pdl no formato ((q)&(p))-(p)
  entPdl <- getLine
  print $ verificaPDLcomEntrada entPdl
