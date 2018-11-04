module Main where

import           Control.Monad
import           Data.Foldable
import           Data.Graph
import           Data.List.Split

vertices' :: Graph -> (Vertex, Vertex)
vertices' grafo = (minimum $ vertices grafo, maximum $ vertices grafo)

uniao :: Graph -> Graph -> Graph
uniao grafo1 grafo2 =
    let vert = vertices' grafo1
        arestas1 = edges grafo1
        arestas2 = edges grafo2
        arestas = arestas1 ++ arestas2
    in  buildG vert arestas

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
