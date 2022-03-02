module RadixTree exposing
    ( empty, singleton, insert, insertWith, insertUnOrdered
    , orderedIntersect, orderedRemove, unOrderedIntersect, unOrderedRemove
    )

{-| Radix tree as defined is defined in Wikipedia: <https://en.wikipedia.org/wiki/Radix_tree>

A data structure that represents a space-optimized trie (prefix tree) in which
each node that is the only child is merged with its parent

It would be easier to follow by example, or try it on a helpful app like
this one: <https://www.cs.usfca.edu/~galles/visualization/RadixTree.html>

Say we have the following list of chars:

[ [ "r", "o", "m", "a", "n", "e" ]
, [ "r", "o", "m", "u", "l", "u", "s" ]
, [ "r", "u", "b", "e", "n", "s" ]
, [ "r", "u", "b", "e", "r" ]
, [ "r", "u", "b", "i", "c", "o", "n" ]
, [ "r", "u", "b", "i", "c", "u", "n", "d", "u", "s" ]
]

We would like to represent this as a Radix tree. Where letters that repeat are
in parent nodes, and the rest are in child nodes.

It would look like this:

  - r
      - ub
          - ic
              - undus
              - on
          - e
              - r
              - ns
      - om
          - ulus
          - ane

This package allows inserting values to the tree in an Ordered and Un-ordered manner.
The ordered one is presented above. The "order" is in relation to the position of each
letter. That is, when we look for the letter "r" that is the beginning of the list
, on an Ordered list, we match it only to other "r" letters that are also at the beginning.

An Un-ordered insert on the other hand doesn't care about the position of the chars, but rather if they exist or not.

Here's an example of how an un-ordered insert would result, assuming we have this list
of Int.

[ [1, 2, 3]
, [2, 1, 4]
, [4, 5, 6]
]

This will result with:

  - 1, 2
      - 3
      - 4
          - 5
          - 6


# Build

@docs empty, singleton, insert, insertWith, insertUnOrdered


# Lists manipulation

@docs orderedIntersect, orderedRemove, unOrderedIntersect, unOrderedRemove

-}

import List.Extra
import Tree
import Tree.Zipper as TreeZipper


{-| Create an empty Radix tree.

    import Tree

    empty
    |> Tree.label --> []

-}
empty : Tree.Tree (List a)
empty =
    Tree.singleton []


{-| Create an Radix tree with a single value.

    import Tree

    singleton [1, 2, 3]
    |> Tree.label --> [1, 2, 3]

-}
singleton : List a -> Tree.Tree (List a)
singleton element =
    Tree.singleton element


{-| An Ordered insert of a value to the Radix tree.
-}
insert : List a -> Tree.Tree (List a) -> Tree.Tree (List a)
insert xs tree =
    insertWith ( orderedIntersect, orderedRemove ) xs tree


{-| An Un-ordered insert of a value to the Radix tree.
-}
insertUnOrdered : List a -> Tree.Tree (List a) -> Tree.Tree (List a)
insertUnOrdered xs tree =
    insertWith ( unOrderedIntersect, unOrderedRemove ) xs tree


{-| Generalized insert, that accepts a tuple with the insert function, and the remove function
to apply.

See the 'split' function.

-}
insertWith : ( List a -> List a -> List a, List a -> List a -> List a ) -> List a -> Tree.Tree (List a) -> Tree.Tree (List a)
insertWith funcs xs tree =
    insertHelper funcs xs (TreeZipper.fromTree tree)
        |> TreeZipper.toTree


{-| Helper function for the insert, which uses recursion to iterate over the tree,
and insert the right elements in the right nodes.
-}
insertHelper : ( List a -> List a -> List a, List a -> List a -> List a ) -> List a -> TreeZipper.Zipper (List a) -> TreeZipper.Zipper (List a)
insertHelper funcs xs treeZipper =
    case TreeZipper.label treeZipper of
        [] ->
            -- We're at the root, which is empty, so we need to check the child.
            case treeZipper |> TreeZipper.forward of
                Nothing ->
                    -- No children, so add value here.
                    treeZipper
                        |> TreeZipper.mapTree (Tree.prependChild (Tree.singleton xs))

                Just nextTreeZipper ->
                    nextTreeZipper
                        |> insertHelper funcs xs

        ys ->
            let
                splitInfo =
                    split funcs xs ys
            in
            if List.isEmpty splitInfo.intersect then
                -- There's no match, try the next sibling, if exists.
                case treeZipper |> TreeZipper.nextSibling of
                    Just nextTreeZipper ->
                        nextTreeZipper
                            -- Recurse.
                            |> insertHelper funcs xs

                    Nothing ->
                        -- We've reached the last sibling, so we can add a new sibling.
                        treeZipper
                            |> TreeZipper.parent
                            |> Maybe.map (TreeZipper.mapTree (Tree.prependChild (Tree.singleton xs)))
                            |> Maybe.withDefault treeZipper

            else if splitInfo.intersect == ys then
                -- Add the remaining of xs under ys.
                if splitInfo.left |> List.isEmpty |> not then
                    if treeZipper |> TreeZipper.children |> List.isEmpty then
                        -- Tree has no children, so we don't need to move forward.
                        -- We can just save the rest of Left.
                        let
                            newTree =
                                treeZipper
                                    |> TreeZipper.tree
                                    |> Tree.prependChild (Tree.singleton splitInfo.left)
                        in
                        treeZipper
                            |> TreeZipper.replaceTree newTree

                    else
                        treeZipper
                            -- Try to advance
                            |> TreeZipper.forward
                            -- Recurse.
                            |> Maybe.map (insertHelper funcs splitInfo.left)
                            |> Maybe.withDefault treeZipper

                else
                    -- Nothing remains to be added.
                    treeZipper

            else
                -- We have a partial match with the existing tree. We need to add a new
                -- parent tree.
                let
                    rightTreeUpdated =
                        treeZipper
                            -- Keep only the diff elements of the Right element.
                            |> TreeZipper.replaceLabel splitInfo.right
                            -- Get the new tree from the focus point.
                            |> TreeZipper.tree

                    newTree =
                        Tree.singleton splitInfo.intersect
                            |> Tree.prependChild rightTreeUpdated
                            |> Tree.prependChild (Tree.singleton splitInfo.left)
                in
                treeZipper
                    |> TreeZipper.replaceTree newTree


{-| Return a record holding the matching elements, the ones the differ from the "left"
(i.e. new items being inserted), and the ones on the "right" (i.e. out of an existing
node of the tree).
-}
split : ( List a -> List a -> List a, List a -> List a -> List a ) -> List a -> List a -> { intersect : List a, left : List a, right : List a }
split ( intersectFunc, removeFunc ) xs ys =
    let
        intersect =
            intersectFunc xs ys

        left =
            removeFunc xs intersect

        right =
            List.foldl (\x accum -> List.Extra.remove x accum) ys intersect
    in
    { intersect = intersect
    , left = left
    , right = right
    }


{-| Find the matching elements, regardless of their order in the list.
-}
unOrderedIntersect : List a -> List a -> List a
unOrderedIntersect xs ys =
    List.foldr
        (\x accum ->
            if List.member x ys then
                x :: accum

            else
                accum
        )
        []
        xs


{-| Remove matching elements, with no regard to of their order in the list. That is,
even if the element is in the middle or end of the list, it will be removed.
-}
unOrderedRemove : List a -> List a -> List a
unOrderedRemove xs intersect =
    List.foldl (\x accum -> List.Extra.remove x accum) xs intersect


{-| Find the matching elements, with regard to their order in the list. That is,
we look for matches starting from the first element.
-}
orderedIntersect : List a -> List a -> List a
orderedIntersect xs ys =
    -- We start with entire list, and trim it down to find the intersecting elements.
    if List.Extra.isPrefixOf xs ys then
        xs

    else if List.length xs == 1 then
        -- No match
        []

    else
        let
            xsUpdated =
                List.take (List.length xs - 1) xs
        in
        orderedIntersect xsUpdated ys


{-| Remove matching elements, with regard to their order in the list. That is,
only if the elements are in the beginning fo the list they will be removed.
-}
orderedRemove : List a -> List a -> List a
orderedRemove xs intersect =
    List.drop (List.length intersect) xs
