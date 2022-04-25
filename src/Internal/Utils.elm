module Internal.Utils exposing
    ( insertWith
    , orderedIntersect
    , orderedRemove
    , unOrderedIntersect
    , unOrderedRemove
    )

{-| Helper functions for the Radix tree implementations.
-}

import List.Extra
import Tree
import Tree.Zipper as TreeZipper


{-| Generalized insert, that accepts a tuple with the insert function, and the remove function
to apply.

See the `split` function.

-}
insertWith : { intersectFunc : List a -> List a -> List a, removeFunc : List a -> List a -> List a } -> List a -> Tree.Tree (List a) -> Tree.Tree (List a)
insertWith funcs xs tree =
    insertHelper funcs xs (TreeZipper.fromTree tree)
        |> TreeZipper.toTree


{-| Helper function for the insert, which uses recursion to iterate over the tree,
and insert the right elements in the right nodes.
-}
insertHelper : { intersectFunc : List a -> List a -> List a, removeFunc : List a -> List a -> List a } -> List a -> TreeZipper.Zipper (List a) -> TreeZipper.Zipper (List a)
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
                    -- Recurse.
                    insertHelper funcs xs nextTreeZipper

        ys ->
            let
                splitInfo =
                    split funcs xs ys
            in
            if List.isEmpty splitInfo.intersect then
                -- There's no match, try the next sibling, if exists.
                case treeZipper |> TreeZipper.nextSibling of
                    Just nextTreeZipper ->
                        -- Recurse.
                        insertHelper funcs xs nextTreeZipper

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
                        let
                            maybeNextTreeZipper =
                                treeZipper
                                    -- Try to advance
                                    |> TreeZipper.forward
                        in
                        case maybeNextTreeZipper of
                            Just nextTreeZipper ->
                                -- Recurse.
                                insertHelper funcs splitInfo.left nextTreeZipper

                            Nothing ->
                                treeZipper

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
split : { intersectFunc : List a -> List a -> List a, removeFunc : List a -> List a -> List a } -> List a -> List a -> { intersect : List a, left : List a, right : List a }
split { intersectFunc, removeFunc } xs ys =
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

    else
        let
            listLength =
                List.length xs
        in
        if listLength == 1 then
            -- No match
            []

        else
            let
                xsUpdated =
                    List.take (listLength - 1) xs
            in
            orderedIntersect xsUpdated ys


{-| Remove matching elements, with regard to their order in the list. That is,
only if the elements are in the beginning of the list they will be removed.
-}
orderedRemove : List a -> List a -> List a
orderedRemove xs intersect =
    List.drop (List.length intersect) xs
