-- from http://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.pdf

-- nodes are represented by distinct integers
type Node = Int

-- a type representing an adjacent edge (either an incoming and outgoing
-- edges of a given Node)
type Adj b = [(b, Node)]

-- a context is a tuple consisting of the incoming adjacent edges,
-- the node itself, a value stored at the node, and the outgoing
-- adjacent edges
type Context a b = (Adj b, Node, a, Adj b)



data Graph a b = Empty | Context a b & Graph a b

data (&) :: 



-- helper methods
isEmpty :: Graph -> Bool
isEmpty Empty = True
isEmpty _ = False

-- get a list of nodes not yet in the graph, where i is
-- the number of nodes desired
newNodes = Int -> Graph a b -> [Node]
newNodes i g = [n+1..n+i] where n = foldr max 0 (nodes g)

gmap :: (Context a b -> Context c d) -> Graph a b -> Graph c d
gmap f Empty = Empty
gmap f (c & g) = f c & gmap f g

grev :: Graph a b -> Graph a b
grev gmap swap where swap (p, v, l, s) = (s, v, l, p)


