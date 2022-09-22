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
    div []
        [ Html.text model.errorMsg
        , Tree.restructure labelToHtml toListItems model.tree
        ]


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