{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char (isDigit)
import System.IO
import Data.List as List
import Data.Maybe as Maybe
import Control.Applicative
--Llista de Nodes, llista d'arestes i alguna estructura que mapegi aresta a par de vertex??
--Llista de propietats, i estructura que mapegi Nodes/arestes (Strings!!) a propietats??
data Date = Date { day::Int,
                    month::Int,
                    year::Int}
    deriving (Eq)
    
data PropVal  = ValInt Int | ValDouble Double | ValString String | ValBool Bool | ValDate Date
    deriving Eq

data Identifier = Identifier {
    ident :: String,
    label :: Label
}

data Graph = Graph {
    nodes   ::  [Node], --guarda els identificadors i les Label de cada node
    edges   ::  [Edge], -- guarda identificadors i Label de cada aresta
    edge    ::  String -> Maybe (Vertex, Vertex), --diccionari per mapejar edge a (v,v)
    properties :: [(Prop, Type)], --llista de propietats amb els seus respectius tipus
    propV    :: (Vertex, Prop) -> Maybe(PropVal), --diccionari per mapejar Vertex a Propietats
    propE   ::  (String, Prop) -> Maybe(PropVal) --Same amb arestes
}



emptyDic = const Nothing

type Type = String
type Node = Identifier
type Edge = Identifier
type Prop = String
type EdgePointer = Graph -> Edge -> (Node, Node)

type Vertex = String --identificador per string
type Label = String
type Element = Int --element hauria de ser una especie de struct

instance Eq Identifier where
    (Identifier i1 l1) == (Identifier i2 l2) = i1 == i2

instance {-# OVERLAPS #-} Show (Either Graph String) where
    show (Left g) = show g
    show (Right s) = s

instance {-# OVERLAPS #-} Show Graph where
    show g = (showAllVertex g) ++ "\n" ++ (showAllEdges g)

            --  ++ "\tVertices: " ++ "{" ++ List.intercalate "," [show v | v <- vertices g] ++ "}" ++ "\n"
            --  ++ "\tEdges: " ++ "{" ++ List.intercalate "," [show e | e <- edges g] ++ "}" ++ "\n"
instance {-# OVERLAPS #-} Show Identifier where
    show i = "(" ++ ident i ++ ", " ++ label i ++ ")"

instance {-# OVERLAPS #-} Show PropVal where
    show (ValInt a) = show a
    show (ValBool b) = show b
    show (ValDouble c) = show c
    show (ValString s) = show s
    show (ValDate d) = show d

instance Show Date where
    show (Date day month year) = (show day) ++ "/" ++ (show month) ++ "/" ++ (show year)

instance Read Date where
    readsPrec _ input = 
        let (days, rest1) = span isDigit input
            day = read days :: Int
            (months, rest2) = span isDigit (drop 1 rest1)
            month = read months :: Int
            year = read (drop 1 rest2) :: Int
            in [((Date day month year), "")]

emptyGraph :: Graph
emptyGraph = Graph [] [] (emptyDic::(String -> Maybe (Vertex, Vertex))) [] 
                                                            (emptyDic::((Vertex, Prop) -> Maybe (PropVal)))
                                                            (emptyDic::((String, Prop) -> Maybe (PropVal)))
--Builds graph from file names
populate :: String -> String -> String -> String -> IO Graph
populate rhofile lambdafile sigmafile propfile = 
    do
        rhoContent <- map words . lines <$> readFile rhofile
        lambdaContent <- map words . lines <$> readFile lambdafile
        sigmaContent <- map words . lines <$> readFile sigmafile
        propContent <- map words . lines <$> readFile propfile

        let rhoParsed = map parseRho rhoContent
        let lambdaParsed = map parseLambda lambdaContent
        let propParsed = map parseProp propContent
        let sigmaParsed = map (\x -> parseSigma x propParsed) sigmaContent

        return (populateLists  rhoParsed lambdaParsed sigmaParsed propParsed)
    where 
        mapExtra :: (a -> b -> c) -> [a] -> b -> [c]
        mapExtra f l x = foldr(\p q -> (f p x):q) [] l
        parseRho :: [String] -> (String, (Vertex, Vertex))
        parseRho (e:v1:v2:_) = (e, (v1, v2))

        parseLambda :: [String] -> (String, Label)
        parseLambda (x:l:_) = (x, l)

        parseProp :: [String] -> (Prop, Type)
        parseProp (p:t:_) = (p, t)

        parseSigma :: [String] -> [(Prop, Type)] -> (String, (Prop, PropVal))
        parseSigma (x:p:v:_) l
            | (any (\(a, b) -> a == p) l) && (snd (head $ filter (\(a, b) -> a == p) l) == "Int") = (x, (p, ValInt (read v :: Int)))
            | (any (\(a, b) -> a == p) l) && (snd (head $ filter (\(a, b) -> a == p) l) == "Double") = (x, (p, ValDouble (read v :: Double)))
            | (any (\(a, b) -> a == p) l) && (snd (head $ filter (\(a, b) -> a == p) l) == "String") = (x, (p, ValString v))
            | (any (\(a, b) -> a == p) l) && (snd (head $ filter (\(a, b) -> a == p) l) == "Bool") = (x, (p, ValBool (read v :: Bool)))
            | (any (\(a, b) -> a == p) l) && (snd (head $ filter (\(a, b) -> a == p) l) == "Date") = (x, (p, ValDate (read v :: Date)))
            | otherwise = ([], ([], ValInt 0))


    --handlerho <- openFile rhofile ReadMode;
    --handlelambda <- openFile lambdafile ReadMode;
--Receives the parsed documents and builds a Graph
populateLists :: [(String, (Vertex, Vertex))] -> [(String, Label)] -> [(String, (Prop, PropVal))]-> [(Prop, Type)] -> Graph
populateLists  rho lambda sigma props = 
        let
                g = buildStructure rho
                g2 = addLables g lambda
                g3 = addPropertyList g2 props
                in setProperties g3 sigma
        where
            buildStructure :: [(String, (Vertex, Vertex))] -> Graph
            buildStructure r  =
                let 
                    gb =  emptyGraph
                    gb2 = addNodeList gb vs1
                    gb3 = addNodeList gb2 vs2
                    in addEdgeList gb3 idents vspair
                where
                    idents = map fst r
                    vspair = map snd r
                    vs1 = map fst vspair
                    vs2 = map snd vspair
            addLables :: Graph -> [(String, Label)] -> Graph
            addLables g [] = g
            addLables g l = foldl (\g (x, y) -> if (existsNode x g) 
                                                then (defVlabel g x y)
                                                else (defElabel g x y)) g l
            setProperties:: Graph -> [(String, (Prop, PropVal))] -> Graph
            setProperties g [] = g
            setProperties g l = foldl (\g (x, y) -> if (existsNode x g)
                                                            then (defVprop g x y)
                                                            else (defEprop g x y)) g l    


--Shows information on ALL vertices
showAllVertex :: Graph -> String
showAllVertex g
    | isEmpty g = []
    |otherwise = showAllVertexAux g (vertices g)
    where 
        showAllVertexAux g [] = []
        showAllVertexAux g (x:xs) = (showVertexData g x) ++ "\n" ++ (showAllVertexAux g xs)

--Shows information on ALL edges
showAllEdges :: Graph -> String
showAllEdges g
    | isEmpty g = []
    |otherwise = showAllEdgesAux g (edgeIdents g)
    where 
        showAllEdgesAux g [] = []
        showAllEdgesAux g (x:xs) = (showEdgeData g x) ++ "\n" ++ (showAllEdgesAux g xs)

--Shows ALL properties of a node
showVertexProperties :: Graph -> Vertex -> String 
showVertexProperties g v 
    | isEmpty g = []
    | not(existsNode v g) = []
    | otherwise = show(showVertexPropertiesAux v (properties g))
    where 
        showVertexPropertiesAux :: Vertex -> [(Prop, Type)] -> [(Prop, PropVal)]
        showVertexPropertiesAux v [] = []
        showVertexPropertiesAux v (x:xs)
            | (propV g (v, (fst x))) /= Nothing = ((fst x), Maybe.fromJust(propV g (v, (fst x)))):showVertexPropertiesAux v xs
            | otherwise = showVertexPropertiesAux v xs
--Shows data of a node
showVertexData :: Graph -> Vertex -> String
showVertexData g v
    | existsNode v g = v ++ "[" ++ label(getNode v g) ++ "]"
                         ++ "{" ++ showVertexProperties g v ++ "}"
    | otherwise = []
--Shows data of an edge
showEdgeData :: Graph -> String -> String
showEdgeData g e
    |existsEdge e g = "(" ++ v1 ++ ")" ++ "--" ++ e ++ "[" ++ label(getEdge e g) ++ "]"
                         ++ "-->" ++ "(" ++ v2 ++ ") {" ++ showEdgeProperties g e ++ "}"
    |otherwise = []
    where
        ns = Maybe.fromMaybe ([], []) (edge g (ident(getEdge e g)))
        v1 = fst ns
        v2 = snd ns
--Shows ALL properties of an edge
showEdgeProperties :: Graph -> String -> String
showEdgeProperties g e
    | isEmpty g = []
    | not(existsEdge e g) = []
    | otherwise = show(showEdgePropertiesAux e (properties g))
    where 
        showEdgePropertiesAux :: String -> [(Prop, Type)] -> [(Prop, PropVal)]
        showEdgePropertiesAux e [] = []
        showEdgePropertiesAux e (x:xs)
            | (propE g (e, (fst x))) /= Nothing = ((fst x), Maybe.fromJust(propE g (e, (fst x)))):showEdgePropertiesAux e xs
            | otherwise = showEdgePropertiesAux e xs

--Prints graph
showGraph :: Graph -> IO ()
showGraph g = putStrLn (show g)
--Returns the identifiers of all edges
edgeIdents :: Graph -> [String]
edgeIdents g = List.map ident (edges g)
--Returns the identifiers of all nodes
vertices :: Graph -> [Vertex]
vertices g = List.map ident (nodes g)
--Gets an adge from it's identifier
getEdge :: String -> Graph -> Edge
getEdge e g
    | e == ident(head (edges g)) = head (edges g)
    | otherwise = getEdge e (Graph (nodes g) (tail (edges g)) (edge g) (properties g) (propV g) (propE g))
--Gets a node from it's identifier
getNode :: Vertex -> Graph -> Node
getNode v g
    |v == ident(head (nodes g)) = head (nodes g)
    |otherwise = getNode v (Graph (tail (nodes g)) (edges g) (edge g) (properties g) (propV g) (propE g))
--MultiPurpose dictionary
addKeyValue :: Eq a => a -> b -> (a -> Maybe b) -> (a -> Maybe b)
addKeyValue key val dict = \k -> if k == key
                                    then Just val
                                    else dict k
--MultiPurpose dictionary
removeKey :: Eq a => a -> (a -> Maybe b) -> (a -> Maybe b)
removeKey key dict = \k -> if k == key
                               then Nothing
                               else dict k
--Tells if graph is empty
isEmpty :: Graph -> Bool
isEmpty g
    | nodes g == [] = True
    | otherwise = False
--Tells if node exists in graph
existsNode :: Vertex -> Graph -> Bool
existsNode v g
    | isEmpty g = False
    | otherwise = any (\x -> x == v) (vertices g)
--Tells if edge exists in graph
existsEdge :: String -> Graph -> Bool
existsEdge e g
    | isEmpty g = False
    | otherwise = any (\x -> (ident x) == e) (edges g)
--Tells if edge connecting two vertices exists and with certain label
existsEdge2Label :: Vertex -> Vertex -> Label -> Graph -> Bool
existsEdge2Label v1 v2 l g
    | (existsNode v1 g) && (existsNode v2 g) = existsEdge2Aux v1 v2 g (edgeIdents g)
    | otherwise = False
    where
        existsEdge2Aux _ _ _ [] = False
        existsEdge2Aux v1 v2 g (e:es)
            | (fst (Maybe.fromJust(edge g e)) == v1) && (snd (Maybe.fromJust(edge g e)) == v2) = 
                ((label (getEdge e g)) == l)
            | otherwise = existsEdge2Aux v1 v2 g es
--Tells if edge connecting two vertices exists
existsEdge2 :: Vertex -> Vertex -> Graph -> Bool
existsEdge2 v1 v2 g
    | (existsNode v1 g) && (existsNode v2 g) = existsEdge2Aux v1 v2 g (edgeIdents g)
    | otherwise = False
    where
        existsEdge2Aux _ _ _ [] = False
        existsEdge2Aux v1 v2 g (e:es)
            | (fst (Maybe.fromJust(edge g e)) == v1) && (snd (Maybe.fromJust(edge g e)) == v2) = True
            | otherwise = existsEdge2Aux v1 v2 g es
--Tells if property exists
existsProperty :: Prop -> Graph -> Bool
existsProperty p g
    | properties g == [] = False
    | otherwise = any (\(x, y) -> x == p) (properties g)
--Adds a list of properties to the graph
addPropertyList:: Graph -> [(Prop, Type)] -> Graph
addPropertyList g l = foldl addProperty g l
--Adds a list of nodes to the graph
addNodeList:: Graph -> [Vertex] -> Graph
addNodeList g = foldl addNode g

foldl2 :: (a -> b -> c -> d -> a) -> a -> [b] -> [(c, d)] -> a
foldl2 _ x [] [] = x
foldl2 f x (l1:ls1) (l2:ls2) = foldl2 f (f x l1 (fst l2) (snd l2)) ls1 ls2
--Adds a list of edges to the graph
addEdgeList:: Graph -> [String] -> [(Vertex, Vertex)] -> Graph
addEdgeList g es vs = foldl2 addEdge g es vs
--Adds a single node to the graph
addNode :: Graph -> Vertex -> Graph
addNode g n
    | existsNode n g = g
    | otherwise = Graph ((nodes g) ++ [(Identifier n [])]) (edges g) (edge g) (properties g) (propV g) (propE g)
--Adds an edge to the graph IF the two nodes exist.
addEdge :: Graph -> String -> Vertex -> Vertex -> Graph
addEdge g e v1 v2
    | (not (existsEdge e g)) && (existsNode v1 g) && (existsNode v2 g) = 
        Graph (nodes g) ((edges g) ++ [Identifier e []]) (addKeyValue e (ident(getNode v1 g), ident(getNode v2 g)) (edge g)) (properties g) (propV g) (propE g)
    | otherwise = g
--Adds a property to the graph
addProperty :: Graph -> (Prop, Type) -> Graph
addProperty g (p, t)
    | not(existsProperty p g) = Graph (nodes g) (edges g) (edge g) ((p, t):properties g) (propV g) (propE g)
    | otherwise = g
--Shows the type of the property in a graph
propertyType :: Graph -> Prop -> Type
propertyType g p
    | existsProperty p g = propertyTypeAux p (properties g)
    |otherwise = []
    where propertyTypeAux p (x:xs)
            | p == (fst x) = snd x
            | otherwise = propertyTypeAux p xs

isCorrectType :: Type -> Bool
isCorrectType t = (t == "Int") || (t == "Double") || (t == "String") || (t == "Bool") || (t == "Date")
--Defines a label for a node if the node doesn't have one
defVlabel :: Graph -> Vertex -> Label -> Graph -- FALTA FER-HO EITHER PER RETORNAR EL ERROR!!
defVlabel g v l
    |existsNode v g = Graph (defVlabelAux v l (nodes g)) (edges g) (edge g) (properties g) (propV g) (propE g)
    |otherwise = g
    where 
        defVlabelAux :: Vertex -> Label -> [Node] -> [Node]
        defVlabelAux _ _ [] = []
        defVlabelAux v lab (x:xs)
            | ((ident x) == v) && (label(getNode v g) == []) = (Identifier v lab):xs
            | otherwise = x:defVlabelAux v lab xs
--Defines a label for an edge if the node doesn't have one
defElabel :: Graph -> String -> Label -> Graph --either
defElabel g e l
    |existsEdge e g = Graph (nodes g) (defElabelAux e l (edges g)) (edge g) (properties g) (propV g) (propE g)
    |otherwise = g
    where 
        defElabelAux :: String -> Label -> [Node] -> [Node]
        defElabelAux _ _ [] = []
        defElabelAux e lab (x:xs)
            | ((ident x) == e) && (label(getEdge e g) == []) = (Identifier e lab):xs
            | otherwise = x:defElabelAux e lab xs
--Updates a property for a node IF the property is available to the graph
defVprop :: Graph -> Vertex -> (Prop, PropVal) -> Graph
defVprop g v (p, val)
    | (existsNode v g) && (existsProperty p g) = 
        Graph (nodes g) (edges g) (edge g) (properties g) (addKeyValue (v, p) val (propV g)) (propE g)
    |otherwise = g
--Updates a property for an edge IF the property is available to the graph
defEprop :: Graph -> String -> (Prop, PropVal) -> Graph
defEprop g e (p, val)
    | (existsEdge e g) && (existsProperty p g) = 
        Graph (nodes g) (edges g) (edge g) (properties g)  (propV g) (addKeyValue (e, p) val (propE g))
    | otherwise = g
--Tells if grah has property
hasProperty :: Graph -> String -> Prop -> (Either (Prop, PropVal)  String)
hasProperty g x p
    |existsNode x g= if((propV g (x,p)) /= Nothing) then Left(p, (Maybe.fromJust(propV g (x,p)))) else Right(x ++ "has no property " ++ p)
    |existsEdge x g= if((propE g (x,p)) /= Nothing) then Left(p, (Maybe.fromJust(propE g (x,p)))) else Right(x ++ "has no property " ++ p)
    |otherwise = Right "No such edge or vertex exists"

--Gives all vertices with certain property
getAllVertWithProp :: Graph  -> Prop -> [(Label, PropVal)]
getAllVertWithProp g p = getAllVertWithPropAux g (vertices g) p
    where 
        getAllVertWithPropAux :: Graph -> [Vertex] -> Prop -> [(Label, PropVal)]
        getAllVertWithPropAux g [] _ = []
        getAllVertWithPropAux g (x:xs) p
            | propV g (x, p) /= Nothing = ((label (getNode x g)), (Maybe.fromJust(propV g (x, p)))):
                                                                                (getAllVertWithPropAux g xs p)
            |otherwise = (getAllVertWithPropAux g xs p)
--Gives all edges with a certain property
getAllEdgeWithProp :: Graph  -> Prop -> [(Label, PropVal)]
getAllEdgeWithProp g p = getAllEdgeWithPropAux g (edgeIdents g) p
    where 
        getAllEdgeWithPropAux :: Graph -> [Vertex] -> Prop -> [(Label, PropVal)]
        getAllEdgeWithPropAux g [] _ = []
        getAllEdgeWithPropAux g (x:xs) p
            | propE g (x, p) /= Nothing = ((label (getEdge x g)), (Maybe.fromJust(propE g (x, p)))):
                                                                                (getAllEdgeWithPropAux g xs p)
            |otherwise = (getAllEdgeWithPropAux g xs p)

 -- List of adjacent nodes of a given vertex -- adjacent meaning que hi ha cami de v a u
adjacents :: Vertex -> Graph -> [Vertex]
adjacents v g
    | existsNode v g = adjacentVerts v (edgeIdents g)
    | otherwise = []
    where   adjacentVerts v [] = []
            adjacentVerts v (x : xs)
                | fst (Maybe.fromJust(edge g x)) == v = (snd (Maybe.fromJust(edge g x))) : adjacentVerts v xs
                | otherwise = adjacentVerts v xs

-- Verify that a path between two nodes exists. Self-loop are not considered
reachable :: Vertex -> Vertex -> Label -> Graph -> Bool
reachable u v l g
    | (existsNode u g) && (existsNode v g) = path v [u] [u] l g 
    | otherwise = False
    where   
        path _ _ [] _ _ = False
        path u visited (n : ns) l g
            | (not (existsEdge2Label n u l g)) && (not (any (\x -> x == u)(ns))) = next
            | otherwise = True
            where   next = path u (visited ++ [n]) (inQueueNodes n (visited ++ [n]) (adjacents n g) ns) l g
                    inQueueNodes :: Vertex -> [Vertex] -> [Vertex] -> [Vertex] -> [Vertex]
                    inQueueNodes v vs [] ns = ns
                    inQueueNodes v vs adj ns = ns ++
                        (filter (\x -> (existsEdge2Label v x l g))
                            (filter (\x -> (not (any (\y->x == y || v == x) ns))) 
                                (filter (\x -> notElem x vs) adj)))

adjacentsK :: (Vertex, Int) -> Graph -> [(Vertex, Int)]
adjacentsK (v, k) g
    | existsNode v g = adjacentVerts v (edgeIdents g)
    | otherwise = []
    where   adjacentVerts v [] = []
            adjacentVerts v (x : xs)
                | fst (Maybe.fromJust(edge g x)) == v = ((snd (Maybe.fromJust(edge g x))), (k-1)) : adjacentVerts v xs
                | otherwise = adjacentVerts v xs


kHops :: Int -> Vertex -> Prop -> (PropVal -> PropVal -> Bool) -> PropVal -> Graph -> (Vertex, Label, PropVal)
kHops k vert p f val g
    | (existsNode vert g) && (existsProperty p g) = path k f val p [(vert, k)] [(vert, k)] g
    | otherwise = ([], [], ValInt 0)
    where
        path :: Int -> (PropVal -> PropVal -> Bool) -> PropVal  -> Prop -> [(Vertex, Int)] -> [(Vertex, Int)] -> Graph -> (Vertex, Label, PropVal)
        path _ _ _ _ _ [] _ = ([], [], ValInt (-1))
        path kp fp valp propp visited (n : ns)  g
            | ((snd n) < 0) || (((propV g ((fst n), propp)) == Nothing) || not((fp valp (Maybe.fromJust(propV g ((fst n), propp)))))) = next
            | otherwise = ((fst n), (label (getNode (fst n) g)), (Maybe.fromJust(propV g ((fst n), propp))))
            where   next = path ((snd n) - 1) fp valp propp (visited ++ [n]) (inQueueNodes (fst n) (visited ++ [n]) (adjacentsK n g) ns) g
                    inQueueNodes :: Vertex -> [(Vertex, Int)] -> [(Vertex, Int)] -> [(Vertex, Int)] -> [(Vertex, Int)]
                    inQueueNodes v vs [] ns = ns
                    inQueueNodes v vs adj ns = ns ++
                            (filter (\x -> (not (any (\y->fst x == fst y || v == fst x) ns))) 
                                (filter (\x -> (not (any (\y->fst x == fst y) vs))) adj))

runCommand :: Graph -> String -> Either Graph String
runCommand g [] = 
    Right( "Wrong Command.\n" ++ 
                "Available commands are:\n" ++
                "\t - addNode v l           -- adds a new node with vertex v and label l\n" ++
                "\t - addEdge e l v1 v2     -- adds a new edge with label l from v1 to v2\n" ++
                "\t - defVprop v prop val   -- updates the property prop for vertex v\n" ++
                "\t - defEprop e prop val   -- same with Edge\n" ++ 
                "\t - showProps x           -- Shows the properties and values of x\n" ++ 
                "\t - reachable v1 v2 l     -- Tells if v2 is reachable from v1 following edges with label l\n" ++
                "\t - kHops k v p f val     -- Shows the tuple (v', lv', valv') where v' is reachable in k hops or less from v\n" ++
                                               "\t\t\t\tand the function f applied to val and the value of property p of v returns true.\n" ++
                                               "\t\t\t\tIMPORTANT: Currently only supports operation ==.")
runCommand g l
    | x == "addNode" = Left (addNodeCommand g xs)
    | x == "addEdge" = Left (addEdgeCommand g xs)
    | x == "defVprop" = Left (defVpropCommand g xs)
    | x == "defEprop" = Left (defEpropCommand g xs)
    | x == "showProps" = Right(showPropsCommand g xs)
    | x == "propV" = Right(proVCommand g xs)
    | x == "propE" = Right(propECommand g xs)
    | x == "reachable" = Right(reachableCommand g xs)
    | x == "kHops" = Right (kHopsCommand g xs)
    |otherwise = Right( "Wrong Command.\n" ++ 
                "Available commands are:\n" ++
                "\t - addNode v l           -- adds a new node with vertex v and label l\n" ++
                "\t - addEdge e l v1 v2     -- adds a new edge with label l from v1 to v2\n" ++
                "\t - defVprop v prop val   -- updates the property prop for vertex v\n" ++
                "\t - defEprop e prop val   -- same with Edge\n" ++ 
                "\t - showProps x           -- Shows the properties and values of x\n" ++ 
                "\t - reachable v1 v2 l     -- Tells if v2 is reachable from v1 following edges with label l\n" ++
                "\t - kHops k v p f val     -- Shows the tuple (v', lv', valv') where v' is reachable in k hops or less from v\n" ++
                                               "\t\t\t\tand the function f applied to val and the value of property p of v returns true.\n" ++
                                               "\t\t\t\tIMPORTANT: Currently only supports operation ==.")
    where
        (x:xs) = words l
        addNodeCommand :: Graph -> [String] -> Graph
        addNodeCommand g (v:l:_)
            |existsNode v g = g
            |otherwise = let
                            g1 = addNode g v
                            in defVlabel g1 v l
        addEdgeCommand:: Graph -> [String] -> Graph
        addEdgeCommand g (e:l:v1:v2:_) = 
            let
                g1 = addEdge g e v1 v2
                in defElabel g1 e l
        
        defVpropCommand :: Graph -> [String] -> Graph
        defVpropCommand g (v:p:val:_)
            |(propertyType g p) == "Int" = defVprop g v (p, ValInt(read val :: Int))
            |(propertyType g p) == "Double" = defVprop g v (p, ValDouble(read val :: Double))
            |(propertyType g p) == "Bool" = defVprop g v (p, ValBool(read val :: Bool))
            |(propertyType g p) == "Date" = defVprop g v (p, ValDate(read val :: Date))
            |(propertyType g p) == "String" = defVprop g v (p, ValString val)
        
        defEpropCommand :: Graph -> [String] -> Graph
        defEpropCommand g (e:p:val:_)
            |(propertyType g p) == "Int" = defEprop g e (p, ValInt(read val :: Int))
            |(propertyType g p) == "Double" = defEprop g e (p, ValDouble(read val :: Double))
            |(propertyType g p) == "Bool" = defEprop g e (p, ValBool(read val :: Bool))
            |(propertyType g p) == "Date" = defEprop g e (p, ValDate(read val :: Date))
            |(propertyType g p) == "String" = defEprop g e (p, ValString val)
        showPropsCommand :: Graph -> [String] -> String
        showPropsCommand g (x:_)
            |existsNode x g= showVertexProperties g x
            |existsEdge x g= showEdgeProperties g x
            |otherwise = "No such edge or vertex exists"
        proVCommand :: Graph -> [String] -> String
        proVCommand g (n:p:_) = show (take (read n :: Int) (getAllEdgeWithProp g p))

        propECommand :: Graph -> [String] -> String
        propECommand g (n:p:_) = show (take (read n :: Int) (getAllEdgeWithProp g p))

        reachableCommand :: Graph -> [String] -> String
        reachableCommand g (v1:v2:l:_) = show $ reachable v1 v2 l g  

        kHopsCommand :: Graph -> [String] -> String
        kHopsCommand g (k:v:p:f:val:_)
                |(f == "==") && ((propertyType g p) == "Int") = show $ kHops (read k :: Int) v p (==) (ValInt(read val :: Int)) g
                |(f == "==") && ((propertyType g p) == "Double") = show $ kHops (read k :: Int) v p (==) (ValDouble(read val :: Double)) g
                |(f == "==") && ((propertyType g p) == "Bool") = show $ kHops (read k :: Int) v p (==) (ValBool(read val :: Bool)) g
                |(f == "==") && ((propertyType g p) == "Date") = show $ kHops (read k :: Int) v p (==) (ValDate(read val :: Date)) g
                |(f == "==") && ((propertyType g p) == "String") = show $ kHops (read k :: Int) v p (==) (ValString val) g
                |otherwise = "Operation not supported."


main = do
    putStrLn "Nom de RHO"
    rhofile <- getLine
    putStrLn "Nom de LAMBDA"
    lambdafile <- getLine
    putStrLn "Nom de SIGMA"
    sigmafile <- getLine
    putStrLn "Nom de PROPERTY"
    propfile <- getLine
    g <- populate "rhoFile.pg" "lambdaFile.pg" "sigmaFile.pg" "propFile.pg"
    showGraph g
    putStrLn ("Welcome to my Property Graph project.\n" ++
                "Available commands are:\n" ++
                "\t - addNode v l           -- adds a new node with vertex v and label l\n" ++
                "\t - addEdge e l v1 v2     -- adds a new edge with label l from v1 to v2\n" ++
                "\t - defVprop v prop val   -- updates the property prop for vertex v\n" ++
                "\t - defEprop e prop val   -- same with Edge\n" ++ 
                "\t - showProps x           -- Shows the properties and values of x\n" ++ 
                "\t - reachable v1 v2 l     -- Tells if v2 is reachable from v1 following edges with label l\n" ++
                "\t - kHops k v p f val     -- Shows the tuple (v', lv', valv') where v' is reachable in k hops or less from v\n" ++
                                               "\t\t\t\tand the function f applied to val and the value of property p of v returns true.\n" ++
                                               "\t\t\t\tIMPORTANT: Currently only supports operation ==.\n"++
             "ATTENTION!!: This script will only update and query on the base graph.\n" ++
             "I could not figure out how to chain updates (maybe with a fold, but I couldn't make it work :'c)\n")
    contents <- getContents
    mapM_ (putStrLn . show . runCommand g) (lines contents)
