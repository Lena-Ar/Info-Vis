module TreeHierarchy exposing (..)

import Browser
import Http
import Json.Decode
import Dict
import Scale
import Statistics
import Html exposing (Html, div, text)
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Attributes exposing (d, class, stroke, strokeWidth, fill, textAnchor, transform, fontFamily, fontSize, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x1, x2, y1, y2, x, y)
import TypedSvg.Types as ST exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Color
import TreeLayout
import Tree exposing (Tree)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        }

type alias Model =
    { tree : Tree String, errorMsg : String }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tree = Tree.singleton "", errorMsg = "Loading ..." }
    , Http.get { url = "../Daten/JSON/XBoxOne_GamesSales_test.json", expect = Http.expectJson GotFlare treeDecoder }
    )


type Msg
    = GotFlare (Result Http.Error (Tree String))


treeDecoder : Json.Decode.Decoder (Tree String)
treeDecoder =
    Json.Decode.map2
        (\name children ->
            case children of
                Nothing ->
                    Tree.tree name []

                Just c ->
                    Tree.tree name c
        )
        (Json.Decode.field "data" (Json.Decode.field "id" Json.Decode.string))
        (Json.Decode.maybe <|
            Json.Decode.field "children" <|
                Json.Decode.list <|
                    Json.Decode.lazy
                        (\_ -> treeDecoder)
        )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFlare (Ok newTree) ->
            ( { model | tree = newTree, errorMsg = "No Error" }, Cmd.none )

        GotFlare (Err error) ->
            ( { model
                | tree = Tree.singleton ""
                , errorMsg =
                    case error of
                        Http.BadBody newErrorMsg ->
                            newErrorMsg

                        _ ->
                            "Some other Error"
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        testTree =
            Tree.tree "root"
                [ Tree.tree "home"
                    [ Tree.tree "user1" []
                    , Tree.tree "user2" []
                    ]
                , Tree.tree "etc" []
                , Tree.tree "var" [ Tree.tree "log" [] ]
                ]

        convertedTestTree : List ( String, Maybe String )
        convertedTestTree =
            testTree
                |> Tree.map (\v -> ( v, Nothing ))
                |> convert
                |> Tree.flatten

        layoutTestTree : Dict.Dict String { x : Float, y : Float }
        layoutTestTree =
            -- layout für Textausgabe berechnen
            Dict.fromList []
    in
    div []
        [ treePlot 1 convertedTestTree
        , Html.div [] [ Html.text "Converted testTree (Child, Maybe Parent)" ]
        , Html.ul [] <|
            List.map
                (\( child, parent ) ->
                    Html.li []
                        [ Html.text <|
                            "(  "
                                ++ child
                                ++ ", "
                                ++ Maybe.withDefault "Nothing" parent
                                ++ ")"
                        ]
                )
                convertedTestTree
        , Html.div [] [ Html.text "Tree Layout" ]
        , Html.ul [] <|
            List.map
                (\( node, { x, y } ) ->
                    Html.li []
                        [ Html.text <|
                            "("
                                ++ node
                                ++ ", x="
                                ++ String.fromFloat x
                                ++ ",y="
                                ++ String.fromFloat y
                                ++ ")"
                        ]
                )
            <|
                Dict.toList layoutTestTree
        ]

--for converting the tree
convert : Tree ( String, Maybe String ) -> Tree ( String, Maybe String )
convert t =
    let
        ( currentLabel, _ ) =
            Tree.label t
    in
    Tree.mapChildren
        (\listChildren ->
            listChildren
                |> List.map
                    (\c ->
                        convert c
                            |> Tree.mapLabel (\( a, _ ) -> ( a, Just currentLabel ))
                    )
        )
        t

--helpers for Tree structure
labelToHtml : String -> Html msg
labelToHtml l =
    Html.text l


toListItems : Html msg -> List (Html msg) -> Html msg
toListItems label children =
    case children of
        [] ->
            Html.li [] [ label ]

        _ ->
            Html.li []
                [ label
                , Html.ul [] children
                ]