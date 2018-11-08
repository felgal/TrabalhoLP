module Main where

import           Control.Monad
import           Data.Bool
import           Data.Char
import           Data.Foldable
import           Data.Graph
import           Data.List
import           Data.List.Split
import           Debug.Trace

removeA :: (Ord a) => a -> [a] -> [a]
removeA _ [] = []
removeA x y
  | x == head y = removeA x $ tail y
  | otherwise = (head y):(removeA x (tail y))

isin :: (Ord a) => a -> [a] -> Bool
isin _ [] = False
isin x y
  | head y == x = True
  | otherwise = isin x (tail y)

elemisin :: (Ord a) => [a] -> [a] -> Bool
elemisin _ [] = False
elemisin [] _ = False
elemisin x y =
  if elem (head x) y
    then
      True
  else
    elemisin (tail x) y

diamond :: Vertex -> Graph -> [Vertex] -> Bool
diamond vertice grafo verdades =
  let execucao = reachable' grafo vertice
  in
    if elemisin execucao verdades
      then
        True
    else
      False

reachable' :: Graph -> Vertex -> [Vertex]
reachable' grafo vertice =
  let vizinhos = reachable grafo vertice
      vizinhos' = removeA vertice vizinhos
      arestas = edges grafo
  in if isin (vertice, vertice) arestas
    then
      vizinhos
    else
      vizinhos'

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
achaDivisao [] _ = 0
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
substituiCharInPDL [] _ _ = []
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
substituiPDL [] _ _ = []
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
      else if (not (c=='<')) && (not (c=='>')) && (not (c=='-')) && (not (c=='&')) && (not (c=='|')) && (not (c=='(')) && (not (c==')')) && (not (c=='0')) && (not (c=='1')) && (not (c=='2')) && (not (c=='3')) && (not (c=='4')) && (not (c=='5')) && (not (c=='6')) && (not (c=='7')) && (not (c=='8')) && (not (c=='9'))
        then ((substituiPDL (substituiCharInPDL pdl c (quantTotal-quantAtual)) (quantAtual-1) quantTotal))
        else ([c]++(substituiPDL (tail pdl) quantAtual quantTotal))

verificaEstado :: Int -> [Int] -> Bool
verificaEstado _ [] = False
verificaEstado estado aceitos=
  let
    estAtual = head(aceitos)
  in if estAtual==estado
    then True
    else False || verificaEstado estado (tail aceitos)

possui :: Int -> [Int] ->Bool
possui x l
         |x`elem` l =True
         |otherwise=False

verificaColchetes :: Graph -> [Int] -> [Int] -> Bool
verificaColchetes _ _ [] = True
verificaColchetes _ [] _ = True
verificaColchetes grafo vizinhos aceitos =
  let
    v = head(vizinhos)
  in if (possui v aceitos)
    then (True && (verificaColchetes grafo (tail vizinhos) aceitos))
    else False

fazPrograma :: [String] ->Graph -> Int ->[Int] ->[Int] ->[Int] ->[[Int]] -> Bool
fazPrograma comandos grafo quantDescida estados arestas estadosAceitos todosEstados=
  let
    estadoAtual = let
      in if (length estados)>0
        then
          (head estados)
        else
          (-1)
    comando = let
      in if (length comandos)>0
        then
          (head comandos)
        else
          "0"
  in if comando=="0" || comandos==[]
    then True
    else if estadoAtual==(-1) || (quantDescida==0)
      then False
      else if (isSubsequenceOf "!" comando)
        then not ( (fazPrograma (tail comandos) grafo quantDescida estados arestas estadosAceitos  todosEstados))
        else if (isSubsequenceOf "?" comando)
          then (verificaEstado estadoAtual (retornaPosVal todosEstados (achaVar (head(tail comandos)))))
          else if (isSubsequenceOf "<" comando)
            then (((verificaEstado estadoAtual estadosAceitos) && (verificaEstado estadoAtual arestas) && (fazPrograma (tail comandos) grafo (quantDescida-1) (reachable grafo estadoAtual) arestas estadosAceitos  todosEstados))  || (fazPrograma comandos grafo (quantDescida-1) (tail estados) arestas estadosAceitos todosEstados))
            else if (isSubsequenceOf "[" comando)
              then (verificaColchetes grafo (reachable' grafo estadoAtual) estadosAceitos) && (fazPrograma (tail comandos) grafo (quantDescida-1) (reachable grafo estadoAtual) arestas estadosAceitos todosEstados)
              else if (isSubsequenceOf "*" comando)
                then (fazPrograma comandos grafo (quantDescida-1) (reachable grafo estadoAtual) arestas estadosAceitos todosEstados) || (fazPrograma (tail comandos) grafo (quantDescida) estados arestas estadosAceitos todosEstados)
                else False

dividePrograma :: String -> [String]
dividePrograma pdl=
  let
    prim = let
      in if (length pdl)>0
        then
          (head pdl)
        else
          '/'
    seg = let
      in if (length pdl)>1
        then
          head (tail pdl)
        else
          '/'
    terc = let
      in if (length pdl)>2
        then
          head (tail( tail pdl))
        else
          '/'
    ult = let
      in if (length pdl)>1
        then
          last (init pdl)
        else
          '/'
  in if ult=='/' || prim=='/' || terc=='/'
    then []
    else if ult=='?'
      then
        (dividePrograma (init( init( init (init pdl))))) ++ ["?"]
      else if prim=='!'
        then ["!"] ++ (dividePrograma ( tail pdl))
        else if terc=='*'
          then ["*",[prim],[seg]] ++ (dividePrograma (tail( tail( tail( tail( tail pdl))))))
          else if terc=='?'
            then ["?",[seg],[prim]] ++ (dividePrograma (tail( tail( tail( tail( tail pdl))))))
            else if prim=='['
              then ["["] ++ (dividePrograma (tail( tail (tail ( tail pdl)))))
              else if prim=='<'
                then ["<"] ++ (dividePrograma (tail( tail (tail ( tail pdl)))))
                else ["<"] ++ (dividePrograma (tail (tail (tail ( tail pdl)))))

achaVar :: String -> Int
achaVar pdl =
  let
    cAtual = let
      in if (length pdl)>0
        then
          head pdl
        else
          '/'
  in if  (cAtual =='0') || (cAtual =='1') ||  (cAtual =='2') ||  (cAtual =='3') ||  cAtual =='4' ||  cAtual =='5' ||  cAtual =='6' ||  cAtual =='7' ||  cAtual =='8'
    then (digitToInt cAtual)
    else if cAtual=='/'
      then (-1)
      else achaVar (tail pdl)

retornaPosVal :: [[Int]] -> Int -> [Int]
retornaPosVal [] _ = []
retornaPosVal vals pos=
  let
    val = head vals
  in if pos==0
    then val
    else retornaPosVal (tail vals) (pos-1)


alcancavelVertices :: Graph ->[Vertex] -> [Vertex]
alcancavelVertices grafo vertices =
  let
    vert = let
      in if (length vertices)>0
        then
          head vertices
        else
          (-1)
  in if vert==(-1)
    then [0]
    else (tail(reachable grafo vert) ++ (alcancavelVertices grafo (tail vertices)))

verificaPDLcomEntrada :: Graph -> String ->[[Int]] -> Bool
verificaPDLcomEntrada grafo pdl posVals =
  let
    posDiv = achaDivisao pdl 0
    conector = pdl !! posDiv
    inicio = head(pdl)
    final = (length pdl)-1
    in if posDiv==0
      then fazPrograma (dividePrograma pdl) grafo (length posVals) (vertices grafo) (alcancavelVertices grafo (vertices grafo)) (retornaPosVal posVals (achaVar pdl)) posVals
      else if posDiv == final
        then if inicio=='!'
          then not(fazPrograma (dividePrograma (take 1(drop 1 pdl))) grafo (length posVals) (vertices grafo) (alcancavelVertices grafo (vertices grafo)) (retornaPosVal posVals (achaVar pdl)) posVals)
          else fazPrograma (dividePrograma (take 1(drop 1 pdl))) grafo (length posVals) (vertices grafo) (alcancavelVertices grafo (vertices grafo)) (retornaPosVal posVals (achaVar pdl)) posVals
        else let
        parte1 = take (posDiv-2) (drop 1 pdl)
        parte2 = drop (posDiv+2) (take 1 pdl)
        in if inicio=='!'
          then if conector=='&'
            then not ((verificaPDLcomEntrada grafo parte1 posVals) && (verificaPDLcomEntrada grafo parte2 posVals))
            else if conector=='|'
              then not ((verificaPDLcomEntrada grafo parte1 posVals) || (verificaPDLcomEntrada grafo parte2 posVals))
              else if conector=='¬'
                then not ((not(verificaPDLcomEntrada grafo parte1 posVals)) || (verificaPDLcomEntrada grafo parte2 posVals))
                else if conector=='-'
                  then not (((verificaPDLcomEntrada grafo parte1 posVals) && (verificaPDLcomEntrada grafo parte2 posVals)) || ((not(verificaPDLcomEntrada grafo parte1 posVals)) && (not(verificaPDLcomEntrada grafo parte2 posVals))) )
                  else False
          else
            if conector=='&'
              then ((verificaPDLcomEntrada grafo parte1 posVals) && (verificaPDLcomEntrada grafo parte2 posVals))
              else if conector=='|'
                then ((verificaPDLcomEntrada grafo parte1 posVals) || (verificaPDLcomEntrada grafo parte2 posVals))
                else if conector=='¬'
                  then ((not(verificaPDLcomEntrada grafo parte1 posVals)) || (verificaPDLcomEntrada grafo parte2 posVals))
                  else if conector=='-'
                    then (((verificaPDLcomEntrada grafo parte1 posVals) && (verificaPDLcomEntrada grafo parte2 posVals)) || ((not(verificaPDLcomEntrada grafo parte1 posVals)) && (not(verificaPDLcomEntrada grafo parte2 posVals))) )
                    else False

encontraErro :: Graph -> String ->[[Int]] -> String
encontraErro grafo pdl posVals =
  let
    posDiv = achaDivisao pdl 0
    conector = pdl !! posDiv
    inicio = head(pdl)
    final = (length pdl)-1
    in if posDiv==0
      then if not (fazPrograma (dividePrograma pdl) grafo (length posVals) (vertices grafo) (alcancavelVertices grafo (vertices grafo)) (retornaPosVal posVals (achaVar pdl)) posVals)
        then pdl
        else []
      else if posDiv == final
        then if inicio=='!'
          then if (fazPrograma (dividePrograma (take 1(drop 1 pdl))) grafo (length posVals) (vertices grafo) (alcancavelVertices grafo (vertices grafo)) (retornaPosVal posVals (achaVar pdl)) posVals)
            then pdl
            else []
          else if (not (fazPrograma (dividePrograma (take 1(drop 1 pdl))) grafo (length posVals) (vertices grafo) (alcancavelVertices grafo (vertices grafo)) (retornaPosVal posVals (achaVar pdl)) posVals))
            then pdl
            else []
        else let
          parte1 = take (posDiv-2) (drop 1 pdl)
          parte2 = drop (posDiv+2) (take 1 pdl)
          in ((encontraErro grafo parte1 posVals) ++ (encontraErro grafo parte2 posVals))



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



  entPdl <- getLine

  --realiza a substituição de caracteres
  novoPdl <- return $ substituiPDL entPdl (length tuplasVars) (length tuplasVars)
  print $ novoPdl

  print $ take (14-2) (drop 1 novoPdl)


  print $ vertices graph
  print $ reachable graph 0
  print $ (alcancavelVertices graph (vertices graph))
  print $ (verificaEstado 0 (alcancavelVertices graph (vertices graph)))
  print $ (verificaEstado 1 (alcancavelVertices graph (vertices graph)))
  print $ (verificaEstado 2 (alcancavelVertices graph (vertices graph)))
  print $ (retornaPosVal varsSemi (achaVar (drop 5(novoPdl))))
  print $ (retornaPosVal varsSemi (achaVar novoPdl))
  --Entra com o pdl no formato ((q)&(p))-(p)
  print $ fazPrograma ["<"] graph (length (vertices graph)) (vertices graph) (alcancavelVertices graph (vertices graph)) (retornaPosVal varsSemi (achaVar novoPdl)) varsSemi
  print $ fazPrograma ["<"] graph (length (vertices graph)) (vertices graph) (alcancavelVertices graph (vertices graph)) (retornaPosVal varsSemi (achaVar (drop 7 novoPdl))) varsSemi
  print $ fazPrograma ["["] graph (length (vertices graph)) (vertices graph) (alcancavelVertices graph (vertices graph)) (retornaPosVal varsSemi (achaVar novoPdl)) varsSemi
  print $ fazPrograma ["["] graph (length (vertices graph)) (vertices graph) (alcancavelVertices graph (vertices graph)) (retornaPosVal varsSemi (achaVar (drop 7 novoPdl))) varsSemi
  print $ (reachable' graph 0)
  print $ possui 1 (retornaPosVal varsSemi (achaVar novoPdl))
  print $ (verificaColchetes graph (reachable' graph 0) (retornaPosVal varsSemi (achaVar novoPdl)))
  print $ fazPrograma ["?","0"] graph (length (vertices graph)) [2] (alcancavelVertices graph (vertices graph)) (retornaPosVal varsSemi (achaVar (novoPdl))) varsSemi
  print $ fazPrograma ["*","<"] graph (length (vertices graph)) [1] (alcancavelVertices graph (vertices graph)) (retornaPosVal varsSemi (achaVar (drop 7 novoPdl))) varsSemi
  print $ verificaPDLcomEntrada graph entPdl varsSemi
  print $ encontraErro graph entPdl varsSemi
