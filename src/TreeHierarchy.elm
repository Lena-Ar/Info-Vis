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
        convertedTree : List ( String, Maybe String )
        convertedTree =
            model.tree
                |> Tree.map (\v -> ( v, Nothing ))
                |> convert
                |> Tree.flatten

        layoutTree : Dict.Dict String { x : Float, y : Float }
        layoutTree =
            TreeLayout.treeLayout 2 convertedTree
    in
    div []
        [ treePlot 1 convertedTree
        , Html.div [] [ Html.text "Hierarchy of publishers, genres and videogames (Child, Maybe Parent)" ]
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
                convertedTree
        , Html.div [] [ Html.text "Hierarchy as Tree Layout" ]
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
                Dict.toList layoutTree
        ]


--plotting tree
treePlot : Float -> List ( String, Maybe String ) -> Svg msg
treePlot minDist tree =
    let
        -- layout berechnen
        xValues : List Float
        xValues =
            -- muss aus dem Layout berechnet werden
            []

        yValues : List Float
        yValues =
            -- muss aus dem Layout berechnet werden
            []

        xScaleLocal : Scale.ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : Scale.ContinuousScale Float
        yScaleLocal =
            yScale yValues
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style []
            [ TypedSvg.Core.text """
            .point circle { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point line { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgba(118, 214, 78,1); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate padding padding ] ]
            ([]
                -- Kanten zeichnen
                ++ []
             -- Knoten zeichnen
            )
        ]


--general settings for visualization
w : Float
w =
    400


h : Float
h =
    200


padding : Float
padding =
    60


radius : Float
radius =
    5.0


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


xScale : List Float -> Scale.ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) <| (Statistics.extent values |> Maybe.withDefault defaultExtent)


yScale : List Float -> Scale.ContinuousScale Float
yScale values =
    Scale.linear ( 0, h - 2 * padding ) <| (Statistics.extent values |> Maybe.withDefault defaultExtent)


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * 10)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )


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