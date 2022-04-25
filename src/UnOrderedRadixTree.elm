module UnOrderedRadixTree exposing
    ( empty, singleton, insert, toTree
    , RadixTreeUnOrdered
    )

{-| Build an unordered Radix tree.


# Structure

@docs UnOrderedRadixTree


# Build

@docs empty, singleton, insert, toTree

-}

import Internal.Utils exposing (insertWith, unOrderedIntersect, unOrderedRemove)
import Tree


{-| Represents an ordered Radix tree. Under the hood we use [Tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/Tree)
-}
type RadixTreeUnOrdered a
    = RadixTreeUnOrdered (Tree.Tree (List a))


{-| Create an empty Radix tree.

    import Tree

    UnOrderedRadixTree.empty
        |> UnOrderedRadixTree.toTree
        |> Tree.label --> []

-}
empty : RadixTreeUnOrdered a
empty =
    Tree.singleton []
        |> RadixTreeUnOrdered


{-| Create an Radix tree with a single value.

    import Tree

    UnOrderedRadixTree.singleton [1, 2, 3]
        |> UnOrderedRadixTree.toTree
        |> Tree.label --> [1, 2, 3]

-}
singleton : List a -> RadixTreeUnOrdered a
singleton element =
    Tree.singleton element
        |> RadixTreeUnOrdered


{-| Extract the `Tree` from the `UnOrderedRadixTree`.
-}
toTree : RadixTreeUnOrdered a -> Tree.Tree (List a)
toTree (RadixTreeUnOrdered tree) =
    tree


{-| An Un-ordered insert of a value to the Radix tree.

    import Tree

    UnOrderedRadixTree.singleton [1, 2, 3]
        |> UnOrderedRadixTree.insert [2, 1, 4]
        |> UnOrderedRadixTree.toTree
        |> Tree.label --> [2, 1]

-}
insert : List a -> RadixTreeUnOrdered a -> RadixTreeUnOrdered a
insert xs (RadixTreeUnOrdered tree) =
    insertWith { intersectFunc = unOrderedIntersect, removeFunc = unOrderedRemove } xs tree
        |> RadixTreeUnOrdered
