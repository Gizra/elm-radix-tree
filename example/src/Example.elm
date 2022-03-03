module Example exposing (..)

import Html exposing (Html, div, li, text, ul)
import Html.Attributes exposing (class)
import RadixTree
import Tree


main : Html msg
main =
    div []
        [ viewRadixList
        , viewRadixTree
        ]


viewRadixList =
    radixList
        |> List.map (\row -> li [] [ text <| Debug.toString row ])
        |> ul []


radixList : List (List String)
radixList =
    [ [ "r", "o", "m", "a", "n", "e" ]
    , [ "r", "o", "m", "u", "l", "u", "s" ]
    , [ "r", "u", "b", "e", "n", "s" ]
    , [ "r", "u", "b", "e", "r" ]
    , [ "r", "u", "b", "i", "c", "o", "n" ]
    , [ "r", "u", "b", "i", "c", "u", "n", "d", "u", "s" ]
    ]


radixExample : Tree.Tree (List String)
radixExample =
    radixList
        |> List.foldl
            (\x accum -> RadixTree.insert x accum)
            RadixTree.empty


viewRadixTree : Html msg
viewRadixTree =
    radixExample
        |> Tree.restructure labelToHtml toListItems
        |> (\root -> Html.ul [] [ root ])


labelToHtml : List String -> Html msg
labelToHtml l =
    l
        |> String.concat
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
