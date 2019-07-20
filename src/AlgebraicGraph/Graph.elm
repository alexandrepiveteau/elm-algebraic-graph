module AlgebraicGraph.Graph exposing
    ( Graph
    , empty
    , singleton
    , union
    , product
    , fromList
    , isEmpty
    , member
    , edges
    , vertices
    , map
    , concatMap
    , fold
    , compact
    )

{-| Graphs can be represented as an algebraic data type through four different primitives : `empty`,
`singleton`, `union` and `product`. Graphs produced with these primitives have the particularity
that they will never be in an inconsistent state, at the expense of possibly including some extra
meta-data that is not really needed. This module has a bunch of functions to help you work with
them!


# Types

@docs Graph


# Construct

@docs empty
@docs singleton
@docs union
@docs product
@docs fromList


# Contents

@docs isEmpty
@docs member
@docs edges
@docs vertices


# Transform

@docs map
@docs concatMap
@docs fold
@docs compact

-}

import Set exposing (Set)



-- ALGEBRAIC GRAPH


{-| Represents a directed graph of values. So `Graph (String)` is a graph of strings and
`Graph (Int)` is a graph of ints.
-}
type Graph a
    = Empty
    | Vertex a
    | Overlay (Graph a) (Graph a)
    | Intersect (Graph a) (Graph a)



-- CONSTRUCTION


{-| Create an empty graph.
-}
empty : Graph a
empty =
    Empty


{-| Create a graph with one value.
-}
singleton : a -> Graph a
singleton node =
    Vertex node


{-| Perform the union of two graphs. Vertices and edges will be unioned.
-}
union : Graph a -> Graph a -> Graph a
union a b =
    Overlay a b


{-| Perform the product of two graphs. Vertices will be unioned, and edges will be created from
each vertex of the left graph to each vertex of the right graph.
-}
product : Graph a -> Graph a -> Graph a
product a b =
    Intersect a b


{-| Create a graph with an edge from the left vertex to the right vertex.
-}
edge : a -> a -> Graph a
edge from to =
    union (singleton from) (singleton to)


{-| Create a graph with no edges from a list of vertex.
-}
fromList : List a -> Graph a
fromList nodes =
    List.foldr
        (\element graph -> union (singleton element) graph)
        empty
        nodes



-- DATA STRUCTURE EXPANSION


{-| Expand this graph to forlm a set of all vertices.
-}
vertices : Graph comparable -> Set comparable
vertices graph =
    case graph of
        Empty ->
            Set.empty

        Vertex node ->
            Set.singleton node

        Overlay a b ->
            Set.union (vertices a) (vertices b)

        Intersect a b ->
            Set.union (vertices a) (vertices b)


{-| Expand this graph to forlm a set of all edges.
-}
edges : Graph comparable -> Set ( comparable, comparable )
edges graph =
    case graph of
        Empty ->
            Set.empty

        Vertex node ->
            Set.empty

        Overlay a b ->
            Set.union (edges a) (edges b)

        Intersect a b ->
            let
                vertA =
                    vertices a
                        |> Set.toList

                vertB =
                    vertices b
                        |> Set.toList

                cross =
                    vertA
                        |> List.concatMap
                            (\x -> List.map (\y -> ( x, y )) vertB)
            in
            cross
                |> Set.fromList
                |> Set.union (edges a)
                |> Set.union (edges b)



-- QUERY


{-| Determine if a graph is empty.
-}
isEmpty : Graph a -> Bool
isEmpty graph =
    case graph of
        Empty ->
            True

        Vertex _ ->
            False

        Overlay a b ->
            isEmpty a && isEmpty b

        Intersect a b ->
            isEmpty a && isEmpty b


{-| Determine if a vertex is in a graph.
-}
member : a -> Graph a -> Bool
member element graph =
    case graph of
        Empty ->
            False

        Vertex a ->
            element == a

        Overlay a b ->
            member element a || member element b

        Intersect a b ->
            member element a || member element b



-- TRANSFORM


{-| Map a function onto a graph, creating a graph with transformed vertices.
-}
map : (a -> b) -> Graph a -> Graph b
map f graph =
    case graph of
        Empty ->
            Empty

        Vertex node ->
            Vertex (f node)

        Overlay a b ->
            Overlay (map f a) (map f b)

        Intersect a b ->
            Intersect (map f a) (map f b)


{-| Map a function onto a graph, and flatten the resulting graphs.
-}
concatMap : (a -> Graph b) -> Graph a -> Graph b
concatMap f graph =
    case graph of
        Empty ->
            Empty

        Vertex node ->
            f node

        Overlay a b ->
            Overlay (concatMap f a) (concatMap f b)

        Intersect a b ->
            Intersect (concatMap f a) (concatMap f b)


{-| Reduce the vertices of a graph.

    fold (+) 0 (fromList [ 1, 2, 3 ]) == 6

-}
fold : (a -> b -> b) -> b -> Graph a -> b
fold operator acc graph =
    case graph of
        Empty ->
            acc

        Vertex node ->
            operator node acc

        Overlay a b ->
            fold operator (fold operator acc a) b

        Intersect a b ->
            fold operator (fold operator acc a) b



-- OPTIMIZATION


{-| Compact a graph, using the intuition that each vertex should result in a new vertex, and each
inferred edge can be compacted into its own small sub-graph.

This removes duplicated edges, and does not find a minimal algebraic graph for a given set of edges
andvertices.

-}
compact : Graph comparable -> Graph comparable
compact graph =
    let
        v =
            graph
                |> vertices
                |> Set.toList
                |> fromList

        e =
            graph
                |> edges
                |> Set.toList
                |> List.map (\( a, b ) -> edge a b)
                |> List.foldr (\g acc -> union g acc) empty
    in
    union v e
