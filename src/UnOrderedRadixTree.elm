module UnOrderedRadixTree exposing
    ( UnOrderedRadixTree, toTree
    , empty, singleton, insert
    )

{-| Build an unordered Radix tree.


# Structure

@docs UnOrderedRadixTree, toTree


# Build

@docs empty, singleton, insert

-}

import Internal.Utils exposing (insertWith, unOrderedIntersect, unOrderedRemove)
import Tree


{-| Represents an ordered Radix tree. Under the hood we use [Tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/Tree)
-}
type UnOrderedRadixTree a
    = UnOrderedRadixTree (Tree.Tree (List a))


{-| Extract the `Tree` from the `UnOrderedRadixTree`.
-}
toTree : UnOrderedRadixTree a -> Tree.Tree (List a)
toTree (UnOrderedRadixTree tree) =
    tree


{-| Create an empty Radix tree.

    import Tree

    UnOrderedRadixTree.empty
        |> UnOrderedRadixTree.toTree
        |> Tree.label --> []

-}
empty : UnOrderedRadixTree a
empty =
    Tree.singleton []
        |> UnOrderedRadixTree


{-| Create an Radix tree with a single value.

    import Tree

    UnOrderedRadixTree.singleton [1, 2, 3]
        |> UnOrderedRadixTree.toTree
        |> Tree.label --> [1, 2, 3]

-}
singleton : List a -> UnOrderedRadixTree a
singleton element =
    Tree.singleton element
        |> UnOrderedRadixTree


{-| An Un-ordered insert of a value to the Radix tree.

    import Tree

    UnOrderedRadixTree.singleton [1, 2, 3]
        -- At this point both "1" and "2" are in both lists. This means that
        -- when converting to tree both "1" and "2" will in the root. The root
        -- will have two children: "3" and "4".
        |> UnOrderedRadixTree.insert [2, 1, 4]
        |> UnOrderedRadixTree.toTree
        |> Tree.label --> [2, 1]

-}
insert : List a -> UnOrderedRadixTree a -> UnOrderedRadixTree a
insert xs (UnOrderedRadixTree tree) =
    insertWith { intersectFunc = unOrderedIntersect, removeFunc = unOrderedRemove } xs tree
        |> UnOrderedRadixTree
