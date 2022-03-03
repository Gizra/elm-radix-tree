module Example exposing (..)

import Html exposing (Html, div, h2, h3, li, text, ul)
import Html.Attributes exposing (class, style)
import RadixTree
import Tree


main : Html msg
main =
    div
        [ style "max-width" "48rem"
        , style "margin-left" "auto"
        , style "margin-right" "auto"
        , style "margin-top" "4rem"
        , style "margin-bottom" "10rem"
        ]
        [ h2 [] [ text "Ordered Insert" ]
        , h3 [] [ text "Original list" ]
        , viewList listString
        , h3 [] [ text "Ordered tree" ]
        , viewTree (listTreeOrdered listString)
        , h2 [] [ text "Un-Ordered Insert" ]
        , h3 [] [ text "Original list" ]
        , viewList listInt
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(2, minmax(0, 1fr))"
            ]
            [ div []
                [ h3 [] [ text "Un-Ordered tree" ]
                , viewTree (listToTreeUnOrdered listInt)
                ]
            , div []
                [ h3 [] [ text "Ordered tree" ]
                , viewTree (listTreeOrdered listInt)
                ]
            ]
        ]


listString : List (List String)
listString =
    [ [ "r", "o", "m", "a", "n", "e" ]
    , [ "r", "o", "m", "u", "l", "u", "s" ]
    , [ "r", "u", "b", "e", "n", "s" ]
    , [ "r", "u", "b", "e", "r" ]
    , [ "r", "u", "b", "i", "c", "o", "n" ]
    , [ "r", "u", "b", "i", "c", "u", "n", "d", "u", "s" ]
    ]


listInt : List (List Int)
listInt =
    [ [ 1, 2, 3 ]
    , [ 2, 1, 4 ]
    , [ 2, 1, 5, 4 ]
    ]


listTreeOrdered : List (List a) -> Tree.Tree (List a)
listTreeOrdered list =
    list
        |> List.foldl
            (\x accum -> RadixTree.insert x accum)
            RadixTree.empty


listToTreeUnOrdered : List (List a) -> Tree.Tree (List a)
listToTreeUnOrdered list =
    list
        |> List.foldl
            (\x accum -> RadixTree.insertUnOrdered x accum)
            RadixTree.empty


viewList : List a -> Html msg
viewList list =
    list
        |> List.map (\row -> li [] [ text <| Debug.toString row ])
        |> ul []


viewTree : Tree.Tree (List a) -> Html msg
viewTree tree =
    tree
        |> Tree.restructure labelToHtml toListItems
        |> (\root -> Html.ul [] [ root ])


labelToHtml : List a -> Html msg
labelToHtml l =
    l
        |> Debug.toString
        |> text


toListItems : Html msg -> List (Html msg) -> Html msg
toListItems label children =
    case children of
        [] ->
            Html.li [] [ label ]

        _ ->
            Html.li []
                [ label
                , Html.ul [ class "list-disc ml-8" ] children
                ]
