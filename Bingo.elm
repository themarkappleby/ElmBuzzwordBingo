module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


-- elm live Bingo.elm --output=bingo.js


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Lesson #" ++ (toString gameNumber)


viewPlayer : String -> Int -> Html msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
        h2 [ id "info", class "classy" ]
            [ playerInfoText ]


viewHeader : String -> Html msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html msg
viewFooter =
    footer []
        [ a [ href "http://google.com" ] [ text "Powered by Elm" ] ]


view : Html msg
view =
    div [ class "content" ]
        [ viewHeader "Elm Tutorial"
        , viewPlayer "Mark" 7
        , viewFooter
        ]


main : Html msg
main =
    view
