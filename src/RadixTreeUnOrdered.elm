module RadixTreeUnOrdered exposing
    ( RadixTreeUnOrdered
    , empty, singleton, insert, toTree
    )

{-| Build an ordered Radix tree.


# Structure

@docs RadixTreeUnOrdered


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

    RadixTreeUnOrdered.empty
        |> RadixTreeUnOrdered.toTree
        |> Tree.label --> []

-}
empty : RadixTreeUnOrdered a
empty =
    Tree.singleton []
        |> RadixTreeUnOrdered


{-| Create an Radix tree with a single value.

    import Tree

    RadixTreeUnOrdered.singleton [1, 2, 3]
        |> RadixTreeUnOrdered.toTree
        |> Tree.label --> [1, 2, 3]

-}
singleton : List a -> RadixTreeUnOrdered a
singleton element =
    Tree.singleton element
        |> RadixTreeUnOrdered


{-| Extract the `Tree` from the `RadixTreeUnOrdered`.
-}
toTree : RadixTreeUnOrdered a -> Tree.Tree (List a)
toTree (RadixTreeUnOrdered tree) =
    tree


{-| An Un-ordered insert of a value to the Radix tree.

    import Tree

    RadixTreeUnOrdered.singleton [1, 2, 3]
        |> RadixTreeUnOrdered.insert [2, 1, 4]
        |> RadixTreeUnOrdered.toTree
        |> Tree.label --> [2, 1]

-}
insert : List a -> RadixTreeUnOrdered a -> RadixTreeUnOrdered a
insert xs (RadixTreeUnOrdered tree) =
    insertWith { intersectFunc = unOrderedIntersect, removeFunc = unOrderedRemove } xs tree
        |> RadixTreeUnOrdered
