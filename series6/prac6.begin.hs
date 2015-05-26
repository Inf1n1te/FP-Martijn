import Prelude
import Data.Maybe
import Data.Char (isDigit)
import Data.List


import Eventloop.EventloopCore
import Eventloop.DefaultConfiguration
import Eventloop.Types.EventTypes

import qualified Eventloop.Module.Websocket.Canvas as C
import qualified Eventloop.Module.BasicShapes as B
import qualified Eventloop.Module.Websocket.Mouse as M
import qualified Eventloop.Module.Websocket.Keyboard as K
import qualified Eventloop.Module.StdOut as S
import Eventloop.Module.Graphs


{- | Start
This function will start the eventloop system using the eventloopConfig
-}
start = startMainloop eventloopConfig

{- | The configuration of the Eventloop system
Uses the graphs module to display graphs. This module
depends on the Keyboard, Mouse, Canvas and BasicShapes modules
-}
eventloopConfig = defaultConfig { moduleConfigurations=[ defaultGraphsModuleConfiguration
                                                       , B.defaultBasicShapesModuleConfiguration
                                                       , C.defaultCanvasModuleConfiguration
                                                       , M.defaultMouseModuleConfiguration
                                                       , K.defaultKeyboardModuleConfiguration
                                                       , S.defaultStdOutModuleConfiguration
                                                       ]}
                where
                    defaultConfig = allModulesEventloopConfiguration beginProgramState eventloop -- Uses beginProgramState and eventloop to build config


{- | ProgramState
This datatype shows which variables are kept
-}
data ProgramState 
    = ProgramState { pressedKey :: [Char]
                   , node1Select :: Maybe Node
                   , node2Select :: Maybe Node
                   , graph :: Graph
                   }                

             
{- | Begingraph
   This is the start state of the graph
-}
beginGraph = Graph allNodes allEdges Directed Unweighted
           where
            allNodes = [ ('a', (50, 50), Red)
                       , ('b', (150, 50), Blue)
                       , ('c', (200, 200), Orange)
                       ]
            allEdges = [ ('a', 'b', Green, 5, Thick)
                       , ('c', 'b', Orange, 3, Thin)
                       , ('c', 'a', Purple, 2, Thin)
                       ]


{-| The beginstate of the ProgramState
-}
beginProgramState = ProgramState [] Nothing Nothing beginGraph
 
 
{- | Instructions
This is the list of all possible instructions
Feel free to add your own
-}
instructions = [ "Instructions"
               , "Press 'n' and click on the screen to create a new node"
               , "Press 'r', click on a node and press a letter to rename the node"
               , "Press 'e', click on two nodes to create an edge"
               , "Press 'd', click on a node to delete the node"
               , "Press 'w', click on two nodes and press a number to weight the edge in between"
               , "Press 'f', click on two nodes to delete an edge"
			   , "Press 'c', click on a node to colour it red"
			   , "Press 'v', click on a node to colour its neighbours blue"
			   , "Press 'x', to reset all colours in the graph"
			   , "Press 'p', click on two nodes to show whether there is a path between those nodes."
			   , "Press 'q' and click to confirm, to see if the graph is strongly connected. all nodes are colored green if this is the case, else they will be colored red."
               , "Press 'esc' to abort the current operation and start another"  
               ]                             
          
                
{- | A variable showing which labels are used for visually added nodes
-}
automaticPossibleLabels :: [Label]
automaticPossibleLabels = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
      

allColors = [Red,Blue,Green,Purple,Grey,Yellow,Orange,Black,White]
       
{- | A function to determine which label can be used next
-}         
nextLabel :: [Node] -> Label
nextLabel nodes
    | null leftOverLabels = error "Did not have a leftover label to give to a node. Please do not create so many nodes!"
    | otherwise = head leftOverLabels
    where
        currentLabels = map (\(l, _, _) -> l) nodes
        leftOverLabels = automaticPossibleLabels \\ currentLabels


nextColor :: Graph -> Color
nextColor g 	| null leftOverColors = error "no leftover colors"
		| otherwise = head leftOverColors
	where
		currentColors = map (\(_,_,c,_,_) -> c) (edges g)
		leftOverColors = allColors \\ (nub currentColors)

                
{- | Add a node to the graph
-}
addNode :: Graph -> Node -> Graph
addNode g@(Graph{nodes=ns}) n = g {nodes=(n:ns)}


{- | Add an edge to the graph
-}
addEdge :: Graph -> Edge -> Graph
addEdge g@(Graph{edges=es}) e = g {edges=(e:es)}


{- | Create an edge based on two nodes
Is drawn from node1 to node2
-}
createEdge :: Node -> Node -> Edge
createEdge (l1, _, c) (l2, _, _)
    = (l1, l2, c, 0, Thin)
    

{- | Finds the edge directed from the first to the second node
-}
findEdgeFromNodeToNode :: Node -> Node -> Graph -> Maybe Edge
findEdgeFromNodeToNode n1 n2 g
    | null possibleEdges = Nothing
    | otherwise = Just $ head possibleEdges
    where
        allEdges = edges g
        possibleEdges = filter (edgeRunsFromNodeToNode n1 n2) allEdges
        

{- | Finds all edges connected to this node
-}                                   
findEdgesAtNode :: Node -> Graph -> [Edge]
findEdgesAtNode (l, _, _) g
    = filter (\(el1, el2, _, _, _) -> el1 == l || el2 == l) allEdges
    where
        allEdges = edges g
      

{- | Find all nodes that are neighbours of a given node
-}
findNeighboringNodes :: Node -> Graph -> [Node]
findNeighboringNodes n@(l, x, t) g 	| directed g == Undirected 	= ns1
					| otherwise			= ns2
	where 	es = findEdgesAtNode n g 
		n1s = map s1 (filter (\(el1,_,_,_,_) -> el1 /= l) es)
		n2s = map s2 (filter (\(_,el2,_,_,_) -> el2 /= l) es)
		s1 (l1,_,_,_,_) = l1
		s2 (_,l2,_,_,_) = l2
		ns1 = filter (\(l1,_,_) -> elem l1 (n1s++n2s)) (nodes g)
		ns2 = filter (\(l1,_,_) -> elem l1 n2s) (nodes g)


{- | Finds all edges that are between two nodes
-}        
findEdgesBetweenNodes :: Node -> Node -> Graph -> [Edge]
findEdgesBetweenNodes n1 n2 g
    = filter (edgeIsBetweenNodes n1 n2)  allEdges
    where
        allEdges = edges g


{- | Conditional to check if an edge is connected to both nodes
-}      
edgeIsBetweenNodes :: Node -> Node -> Edge -> Bool
edgeIsBetweenNodes (l1, _, _) (l2, _, _) (el1, el2, _, _, _)
    = (el1 == l1 && el2 == l2) || (el1 == l2 && el2 == l1)


{- | Conditional to check if the runs is directed from the first
to the second node
-}
edgeRunsFromNodeToNode :: Node -> Node -> Edge -> Bool
edgeRunsFromNodeToNode (l1, _, _) (l2, _, _) (el1, el2, _, _, _)
    = (l1 == el1) && (l2 == el2)

	
{- | Recolors a node in graph g with the color c
-}
colorNode :: Graph -> Color -> Node -> Graph
colorNode g c n@(l, x, _) = (flip addNode) (l,x,c) $ removeNode n g


{- | Colour an edge with specified colour.
-}
colorEdge :: Graph -> Color -> Edge -> Graph
colorEdge g c e@(l1, l2, _, x, y) = (flip addEdge) (l1, l2, c, x, y) $ removeEdge e g


{- | Color a path in the specified color -}
colorPath :: Color -> Graph -> [Edge] -> Graph
colorPath _ g [] 	= g 
colorPath c g (e:es)	= colorPath c g' es
	where	g' = colorEdge g c e


{- | Resets all colors in the given graph
-}
resetColor :: Graph -> Graph
resetColor g = setColor Orange g

setColor :: Color -> Graph -> Graph
setColor color g = g''
    where
    	g''= foldl (ce color) g' (edges g')
	ce c g n = colorEdge g c n
	g' = foldl (cn color) g (nodes g)
	cn c g n = colorNode g c n
       
		
{- | Removes the node from the graph
-}
removeNode :: Node -> Graph -> Graph
removeNode n g 
    = g {nodes = allNodes'}
    where
        allNodes = nodes g
        allNodes' = delete n allNodes

{- | Removes the edge from the graph
-}
removeEdge :: Edge -> Graph -> Graph
removeEdge e g
    = g {edges = allEdges'}
    where
        allEdges = edges g
        allEdges' = delete e allEdges

{- | Removes a node, and all edges connected to it,
from the graph
-}
removeNodeWithAdjoiningEdges :: Node -> Graph -> Graph
removeNodeWithAdjoiningEdges n g
    = g''
    where
        g'  = removeNode n g
        g'' = foldr removeEdge g' (findEdgesAtNode n g) 

{- | Rename a node in the edge to the new label
if the node is connected to that edge
-}        
renameNodeInEdge :: Node -> Label -> Edge -> Edge
renameNodeInEdge (oldL, _, _) newL (el1, el2, color, weight, thickness)
    | oldL == el1 = (newL, el2, color, weight, thickness)
    | oldL == el2 = (el1, newL, color, weight, thickness)
    | otherwise   = (el1, el2, color, weight, thickness)

{- | Check whether a path exists between the two given nodes in the given graph
-}

hasPath :: Graph -> Node -> Node -> Bool
hasPath g n1 n2 | n1 == n2	= True
		| otherwise 	= True `elem` (map (\x -> hasPath g' x n2) (findNeighboringNodes n1 g))
    where
    	g' = removeNode n1 g 


{- | Check whether a graph is strongly connected; that is, when a graph when all paths are traversed from a random given node, it can reach all other nodes.
-}
isStronglyConnected :: Graph -> Bool
isStronglyConnected g@(Graph{nodes=(ns)}) = null (isStronglyConnectedPriv ns g)

{- private function that does the work; function above is a nice wrapper -}
isStronglyConnectedPriv :: [Node] -> Graph -> [Node]
isStronglyConnectedPriv [] _ = []
isStronglyConnectedPriv (n:ns) g = nodes (nodesUnreachable n g) ++ isStronglyConnectedPriv ns g

{- | Returns a graph with all nodes removed that are reachale from the given node -}
nodesUnreachable :: Node -> Graph -> Graph
nodesUnreachable n g 	| null (adjns)		= g'
			| n `elem` (nodes g) 	= foldr (nodesUnreachable) g' adjns
			| otherwise 		= g -- node could have been removed already by other branch
	where	adjns 	= findNeighboringNodes n g
		g'	= removeNodeWithAdjoiningEdges n g
		

colorConnectedGraphs ::	[Color] ->  -- list of colors to give the graphs in order
			Graph ->  -- Graph to change
			[Node] ->  -- nodes still to look for subgraphs in
			Graph
colorConnectedGraphs _ g [] = g
colorConnectedGraphs [] g _ = g
colorConnectedGraphs (color:cs) g z@(n:ns) = colorConnectedGraphs cs g' nns
	where
		nns = filter (\x -> x `elem` (nodes (nodesUnreachable n g))) ns
		ons = filter (\y -> y `notElem` nns) z
		g' = foldl (cn color) g ons
		cn c g n = colorNode g c n

nodeSeqToEdgeSeq :: Graph -> [Node] -> [Edge]
nodeSeqToEdgeSeq g [] = error "bla"
nodeSeqToEdgeSeq g [n] = []
nodeSeqToEdgeSeq g (n1:n2:ns) = edge : nodeSeqToEdgeSeq g (n2:ns)
	where (Just edge) = findEdgeFromNodeToNode n1 n2 g

findAllPaths :: Graph -> Node -> Node -> [[Node]]
findAllPaths g nb ne = filter (\x -> elem ne x) (markAllPaths g ne nb)
		

markAllPaths :: Graph -> Node -> Node -> [[Node]]
markAllPaths g ne nb
	| nb == ne 				= [[nb]]
	| null (findNeighboringNodes nb g) 	= [[]]
	| otherwise				= map (\x -> [nb] ++ x) paths
		where	g' = removeNodeWithAdjoiningEdges nb g
			nbs = findNeighboringNodes nb g
			paths = concat $ map (markAllPaths g' ne) nbs


{- | The eventloop
This function uses the current state and an In event to determine
the new state and what changes should be made as a list of Out events.
-}
eventloop :: ProgramState -> In -> (ProgramState, [Out])

eventloop ps Start
    = (ps, [OutGraphs SetupGraphs, OutGraphs $ DrawGraph (graph ps), OutGraphs $ Instructions instructions])

eventloop ps@(ProgramState "f" (Just node1s) _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | edgeM == Nothing = (ProgramState [] Nothing Nothing g, [])
    | otherwise = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Deleted edge from '" ++ [l1] ++ "' to '" ++ [l2] ++ "'\n"])
    where
        nodeAtPosM = onNode allNodes p
        (Just nodeAtPos) = nodeAtPosM
        allNodes = nodes g
        edgeM = findEdgeFromNodeToNode node1s nodeAtPos g
        (Just edge) = edgeM
        (l1, l2, _, _, _) = edge
        g' = removeEdge edge g
                     
{- | If 'w' has been pressed, two nodes are selected and the next key
is a digit, the edge running from node1s to node2s is weighted as that
digit
-}                    
eventloop ps@(ProgramState "w" (Just node1s) (Just node2s) g) (InGraphs (Key [key]))
    | isDigit key && edgeM /= Nothing = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Weighted edge from '" ++ [l1] ++ "' to '" ++ [l2] ++ "' with " ++ (show weight) ++ "\n"])
    | otherwise   = (ProgramState [] Nothing Nothing g, [])
    where
        edgeM = findEdgeFromNodeToNode node1s node2s g
        (Just edge@(l1, l2, col, w, thick)) = edgeM
        weight = read [key] :: Weight
        edge' = (l1, l2, col, weight, thick)
        g' =  (flip addEdge) edge' $ removeEdge edge g
    
    

{- | If 'd' has been pressed and a node is selected
, the node is deleted from the graph
-}
eventloop ps@(ProgramState "d" _ _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | otherwise = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Deleted node '" ++ [l] ++ "'\n"])
    where
        (l, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        (Just nodeAtPos) = nodeAtPosM
        allNodes = nodes g
        g' = removeNodeWithAdjoiningEdges nodeAtPos g

     
{- | If 'e' has been pressed, a node selected and a new node is selected
an edge is drawn between the two nodes
-}
eventloop ps@(ProgramState "e" (Just node1s) _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | otherwise = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Created edge from '" ++ [l1] ++ "' to '" ++ [l2] ++ "'\n"])
    where
        (l1, _, _) = node1s
        (l2, _, _) = nodeAtPos
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        (Just nodeAtPos) = nodeAtPosM
        g' = addEdge g $ createEdge node1s nodeAtPos
                  
                  
{- | If 'r' has been pressed, a node selected and a new key stroke
comes in, the label of the selected node is changed
-}
eventloop ps@(ProgramState "r" (Just node1s) _ g) (InGraphs (Key [l]))
    = (ProgramState [] Nothing Nothing g'', [OutGraphs $ DrawGraph g'', OutStdOut $ S.StdOutMessage $ "Renamed node '" ++ [oldL] ++ "' to '" ++ [l] ++ "'\n"])
    where
        allNodes = nodes g
        allEdges = edges g
        (oldL, p, color) = node1s
        node' = (l, p, color)
        allEdges' = map (renameNodeInEdge node1s l) allEdges :: [Edge]
        g'  = (flip addNode) node' $ removeNode node1s g
        g'' = g' {edges = allEdges'}


{- | If 'c' has been pressed, a node selected and a new key stroke
comes in, the color of the selected node is changed to red
-}
eventloop ps@(ProgramState "c" (Just node1s) _ g) (_)
    = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Colored node red\n"])
    where
        g' = colorNode g Red node1s


{- | If 'v' has been pressed, a node selected and a new key stroke
comes in, the color of the selected node is changed to red
-}
eventloop ps@(ProgramState "v" (Just node1s) _ g) (_)
    = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Colored adjacent nodes blue\n"])
    where
        g' = foldl (cn Blue) g (findNeighboringNodes node1s g)
	cn c g n = colorNode g c n


{- | If 'x' has been pressed, all nodes' colors is reset to orange
-}
eventloop ps@(ProgramState _ _ _ g) (InGraphs( Key "x"))
    = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Reset all colors\n"])
    where
        g' = resetColor g


{- | If 'n' has been pressed and the mouse has 
clicked at a position where there is no node yet,
a new node is inserted at that point
-}                    
eventloop ps@(ProgramState "n" _ _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Inserted node '" ++ [nextlabel] ++ "'\n"])
    | otherwise             = (ps, [OutStdOut $ S.StdOutMessage "Tried to insert a node on another node"])
    where
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g
        nextlabel = nextLabel allNodes
        newNode = (nextlabel, p, Orange)
        g' = g {nodes=(newNode:allNodes)}
         
         
{- | If 'p' has been pressed, a node selected and a new key stroke
comes in, the color of the selected node is changed to red
-}
eventloop ps@(ProgramState "p" (Just node1s) _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing 	= (ps,[])
    | otherwise 		= (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Check path existence:\n"])
    where
	nodeAtPosM 	= onNode (nodes g) p
	(Just nodeM) 	= nodeAtPosM 
	g' 	| hasPath g node1s nodeM 	= colorNode (colorNode g Green nodeM) Green node1s
		| otherwise 			= colorNode (colorNode g Red nodeM) Red node1s


{- | If 'q' has been pressed, all nodes and edges are colored green if the graph is strongly connected. Else they is colored red.
-}
eventloop ps@(ProgramState "q" _ _ g) (InGraphs (Mouse (Click _) p))
    = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Check if strongly connected.\n"])
    where
	g' 	| isStronglyConnected g = setColor Green g
		| otherwise		= setColor Red g


{- | If 'w' has been pressed, all individual graphs are marked with a color.
-}
eventloop ps@(ProgramState _ _ _ g) (InGraphs (Key "y"))
    = (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Color code subgraphs.\n"])
    where
	g' = colorConnectedGraphs allColors g (nodes g)


{- | If 'u' has been pressed, a node selected and a new key stroke
comes in, the color of the selected node is changed to red
-}
eventloop ps@(ProgramState "u" (Just node1s) _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing 	= (ps,[])
    | otherwise 		= (ProgramState [] Nothing Nothing g', [OutGraphs $ DrawGraph g', OutStdOut $ S.StdOutMessage $ "Mark all " ++ show ess ++ " paths:\n"])
    where
	nodeAtPosM 	= onNode (nodes g) p
	(Just nodeM) 	= nodeAtPosM 
	ess = map (nodeSeqToEdgeSeq g) (findAllPaths g node1s nodeM)
	g' = foldl (\z (x,y) -> colorPath x z y) g (zip allColors ess) 
	cn c g n = colorPath g c n
	



{- | Buffer the last node selected if it doesn't 
trigger an event on first spot
-}
eventloop ps@(ProgramState _ Nothing _ g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [])
    | otherwise             = (ps {node1Select = Just nodeAtPos}, [OutStdOut $ S.StdOutMessage $ "[1st Select] Click on node '" ++ [l] ++ "'\n"])
    where
        (l, _, _) = nodeAtPos
        (Just nodeAtPos) = nodeAtPosM
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g

        
{- | Buffer the last node selected if it doesn't trigger an event on second spot -}
eventloop ps@(ProgramState _ (Just _) Nothing g) (InGraphs (Mouse (Click _) p))
    | nodeAtPosM == Nothing = (ps, [OutStdOut $ S.StdOutMessage "Clicked on not a node\n"])
    | otherwise             = (ps {node2Select = Just nodeAtPos}, [OutStdOut $ S.StdOutMessage $ "[2nd Select] Click on node '" ++ [l] ++ "'\n"])
    where
        (l, _, _) = nodeAtPos
        (Just nodeAtPos) = nodeAtPosM
        nodeAtPosM = onNode allNodes p
        allNodes = nodes g

        
{- | Abort current operation and reset start on "esc" -}
eventloop ps (InGraphs (Key "esc"))
    = (ProgramState [] Nothing Nothing (graph ps), [OutStdOut $ S.StdOutMessage "Aborted current operation\n"])


{- | Stop the system on "s" -}
eventloop ps (InGraphs (Key "s"))
    = (ps, [OutStdOut $ S.StdOutMessage "Stopping system...\n", Stop])
        
        
{- | Buffer the last press key if it doesn't trigger an event -}
eventloop ps@(ProgramState _ _ _ _) (InGraphs (Key key))
    = (ps {pressedKey = key}, [OutStdOut $ S.StdOutMessage $ "Buffered keystroke '" ++ key ++ "'\n" ])
        
         
{- | For all other In events, do nothing -}                                                                            
eventloop ps _ = (ps, []) 


