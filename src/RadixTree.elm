module RadixTree exposing
    ( empty, singleton, insert
    , toTree
    )

{-| Build an ordered Radix tree.


# Build

@docs empty, singleton, insert, insertWith, insertUnOrdered


# Lists manipulation

@docs orderedIntersect, orderedRemove, unOrderedIntersect, unOrderedRemove

-}

import Internal.Utils exposing (insertWith, orderedIntersect, orderedRemove)
import Tree


{-| Represents an ordered Radix tree. Under the hood we use [Tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/Tree)
-}
type RadixTree a
    = RadixTree (Tree.Tree (List a))


{-| Create an empty Radix tree.

    import Tree

    RadixTree.empty
        |> RadixTree.toTree
        |> Tree.label --> []

-}
empty : RadixTree a
empty =
    Tree.singleton []
        |> RadixTree


{-| Create an Radix tree with a single value.

    import Tree

    RadixTree.singleton [1, 2, 3]
        |> RadixTree.toTree
        |> Tree.label --> [1, 2, 3]

-}
singleton : List a -> RadixTree a
singleton element =
    Tree.singleton element
        |> RadixTree


{-| Extract the `Tree` from the `RadixTree`.
-}
toTree : RadixTree a -> Tree.Tree (List a)
toTree (RadixTree tree) =
    tree


{-| An Ordered insert of a value to the Radix tree.

    import Tree

    RadixTree.singleton [1, 2, 3]
        |> RadixTree.insert [1, 2, 4]
        |> RadixTree.toTree
        |> Tree.label --> [1, 2]

    RadixTree.singleton [1, 2, 3]
        |> RadixTree.insert [1, 2, 4]
        |> RadixTree.toTree
        |> Tree.children --> [singleton [4], singleton [3]]

-}
insert : List a -> RadixTree a -> RadixTree a
insert xs (RadixTree tree) =
    insertWith ( orderedIntersect, orderedRemove ) xs tree
        |> RadixTree
