module UnorderedRadixTree exposing
    ( UnorderedRadixTree, toTree
    , empty, singleton, insert
    )

{-| Build an unordered Radix tree.


# Structure

@docs UnorderedRadixTree, toTree


# Build

@docs empty, singleton, insert

-}

import Internal.Utils exposing (insertWith, unOrderedIntersect, unOrderedRemove)
import Tree


{-| Represents an ordered Radix tree.
-}
type UnorderedRadixTree a
    = UnorderedRadixTree (Tree.Tree (List a))


{-| Extract the `Tree` from the `RadixTree`. The Tree is from the package [Tree](https://package.elm-lang.org/packages/zwilias/elm-rosetree/latest/Tree)
that has a very nice API for working with trees, and allows converting the `Tree`
to a `Zipper` - making traversals easier.
-}
toTree : UnorderedRadixTree a -> Tree.Tree (List a)
toTree (UnorderedRadixTree tree) =
    tree


{-| Create an empty Radix tree.

    import Tree

    UnorderedRadixTree.empty
        |> UnorderedRadixTree.toTree
        |> Tree.label --> []

-}
empty : UnorderedRadixTree a
empty =
    Tree.singleton []
        |> UnorderedRadixTree


{-| Create an Radix tree with a single value.

    import Tree

    UnorderedRadixTree.singleton [1, 2, 3]
        |> UnorderedRadixTree.toTree
        |> Tree.label --> [1, 2, 3]

-}
singleton : List a -> UnorderedRadixTree a
singleton element =
    Tree.singleton element
        |> UnorderedRadixTree


{-| An Un-ordered insert of a value to the Radix tree.

    import Tree

    UnorderedRadixTree.singleton [1, 2, 3]
        |> UnorderedRadixTree.insert [2, 1, 4]
        -- At this point both "1" and "2" are in both lists. This means that
        -- when converting to tree both "1" and "2" will in the root. The root
        -- will have two children: "3" and "4".
        |> UnorderedRadixTree.toTree
        -- The name might be a bit confusing, but `Tree.label` gets us the values
        -- of the current node of the tree. In our case the value is a list of Int.
        |> Tree.label --> [2, 1]

-}
insert : List a -> UnorderedRadixTree a -> UnorderedRadixTree a
insert xs (UnorderedRadixTree tree) =
    insertWith { intersectFunc = unOrderedIntersect, removeFunc = unOrderedRemove } xs tree
        |> UnorderedRadixTree
