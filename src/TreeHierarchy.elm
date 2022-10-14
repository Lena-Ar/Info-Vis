module TreeHierarchy exposing (..)

import Browser
import Http
import Json.Decode
import Dict
import Scale
import Statistics
import Html exposing (Html, div, text)
import Html.Attributes exposing (style, href)
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Attributes exposing (d, class, stroke, strokeWidth, fill, textAnchor, transform, fontFamily, fontSize, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x1, x2, y1, y2, x, y)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
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
    , Http.get { url = "../Daten/JSON/XBoxOne_GamesSales_Projekt.json", expect = Http.expectJson GotFlare treeDecoder }
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
    div [Html.Attributes.style "padding" "10px" ]
        [ Html.h1 [Html.Attributes.style "fontSize" "40px"] 
            [Html.text "Visualization of a market analysis for Video Games Sales on XBox One"]
        , Html.h2 [Html.Attributes.style "fontSize" "20px"] 
            [Html.text "Created for course 'Information Retrieval and Visualization' at Uni Halle"]
        , Html.p [Html.Attributes.style "fontSize" "15px"] 
            [Html.text "By clicking on the link below, you can go to more detailed visualizations of the Video Games Sales data to retrieve insights in sales data."]
        , Html.p [Html.Attributes.style "fontSize" "16px"]
            [Html.a [href "MainScatterParallel.elm"] [Html.text "Detailed Plots"]]
        , Html.h1 [Html.Attributes.style "fontSize" "30px"] 
            [Html.text "Tree Diagramm / Tree Hierarchy for Video Games"]
        , Html.p [Html.Attributes.style "fontSize" "15px"] 
            [Html.text "In this hierarchy, you can see all video games, genres and publishers of the dataset. You can see all publishers and for each which genres they offer. Furthermore you can see the games of each publisher assigned beneath their corresponding genre."
            , Html.br [] []
            , Html.text "You as a publisher or stakeholder can remind which genres you offer and which games are of what genre. Of course you can also see and compare what competitors offer."]
        , Html.h3 [Html.Attributes.style "fontSize" "16px"] 
            [Html.text "In the Tree Diagramm / Tree Hierarchy below you can get an overview over the data."]
        , Html.div [] [treePlot2 1 convertedTree]
        , Html.h3 [Html.Attributes.style "fontSize" "16px"] 
            [Html.text "The Tree Diagramm / Tree Hierarchy below shows the same data as the one above but a litter closer so you can explore in more detail."]
        , Html.div [] [treePlot 1 convertedTree]
        , Html.div [Html.Attributes.style "fontSize" "15px"] 
            [ Html.text "Hierarchy of publishers, genres and videogames (Child, Maybe Parent)" ]
        , Html.ul [Html.Attributes.style "fontSize" "10px"] <|
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
        , Html.div [Html.Attributes.style "fontSize" "15px"] 
            [ Html.text "Hierarchy as Tree Layout" ]
        , Html.ul [Html.Attributes.style "fontSize" "10px"] <|
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
        --computing layout
        layout : Dict.Dict String { x : Float, y : Float }
        layout = 
            TreeLayout.treeLayout 2 tree

        xValues : List Float
        xValues =
            Dict.toList layout
                |> List.map (\( a, b ) -> b.x )

        yValues : List Float
        yValues =
            Dict.toList layout
                |> List.map (\( a, b ) -> b.y )

        xScaleLocal : Scale.ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : Scale.ContinuousScale Float
        yScaleLocal =
            yScale yValues

        --dependencies for treePlot to draw lines/paths from parent to child node
        --to get x and y values from parent and child nodes
        nodeValues : List NodeValues
        nodeValues =
            List.map
                (\( node, parent ) ->
                    let
                        childx =
                            Dict.get node layout |> Maybe.map (\a -> a.x) |> Maybe.withDefault -1

                        childy =
                            Dict.get node layout |> Maybe.map (\a -> a.y) |> Maybe.withDefault -1

                        parentx =
                            parent |> Maybe.andThen (\p -> Dict.get p layout) |> Maybe.map (\a -> a.x) |> Maybe.withDefault -1

                        parenty =
                            parent |> Maybe.andThen (\p -> Dict.get p layout) |> Maybe.map (\a -> a.y) |> Maybe.withDefault -1

                        label =
                            node
                    in
                    NodeValues childx childy parentx parenty label
                )
                tree
        --to avoid root node getting a path as it has no parent node
        checkRootNegative data = 
            if (data.parentx < 0) && (data.parenty < 0) then
                NodeValues 0 1 data.childx data.childy data.label
            else
                NodeValues data.childx data.childy data.parentx data.parenty data.label

        nodeValuesPath : List NodeValues
        nodeValuesPath =
            List.map checkRootNegative nodeValues
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 180, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style []
            [ TypedSvg.Core.text """
            .point circle { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point line { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgba(0, 204, 0,1); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate padding padding ] ]
            (List.map (line xScaleLocal yScaleLocal) nodeValuesPath)
        , g
            [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) nodeValues)
        ]

treePlot2 : Float -> List ( String, Maybe String ) -> Svg msg
treePlot2 minDist tree =
    let
        --computing layout
        layout : Dict.Dict String { x : Float, y : Float }
        layout = 
            TreeLayout.treeLayout 2 tree

        xValues : List Float
        xValues =
            Dict.toList layout
                |> List.map (\( a, b ) -> b.x )

        yValues : List Float
        yValues =
            Dict.toList layout
                |> List.map (\( a, b ) -> b.y )

        xScaleLocal : Scale.ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : Scale.ContinuousScale Float
        yScaleLocal =
            yScale yValues

        --dependencies for treePlot to draw lines/paths from parent to child node
        --to get x and y values from parent and child nodes
        nodeValues : List NodeValues
        nodeValues =
            List.map
                (\( node, parent ) ->
                    let
                        childx =
                            Dict.get node layout |> Maybe.map (\a -> a.x) |> Maybe.withDefault -1

                        childy =
                            Dict.get node layout |> Maybe.map (\a -> a.y) |> Maybe.withDefault -1

                        parentx =
                            parent |> Maybe.andThen (\p -> Dict.get p layout) |> Maybe.map (\a -> a.x) |> Maybe.withDefault -1

                        parenty =
                            parent |> Maybe.andThen (\p -> Dict.get p layout) |> Maybe.map (\a -> a.y) |> Maybe.withDefault -1

                        label =
                            node
                    in
                    NodeValues childx childy parentx parenty label
                )
                tree
        --to avoid root node getting a path as it has no parent node
        checkRootNegative data = 
            if (data.parentx < 0) && (data.parenty < 0) then
                NodeValues 0 1 data.childx data.childy data.label
            else
                NodeValues data.childx data.childy data.parentx data.parenty data.label

        nodeValuesPath : List NodeValues
        nodeValuesPath =
            List.map checkRootNegative nodeValues
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ TypedSvg.style []
            [ TypedSvg.Core.text """
            .point circle { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point line { stroke: rgba(100, 100, 100,1); fill: rgba(100, 100, 100,1); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgba(0, 204, 0,1); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate padding padding ] ]
            (List.map (line xScaleLocal yScaleLocal) nodeValuesPath)
        , g
            [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) nodeValues)
        ]

--list of floats for values of child and parent x and y and label as string
type alias NodeValues =
    { childx : Float
    , childy : Float
    , parentx : Float
    , parenty : Float
    , label : String
    }

--for drawing line
line scaleX scaleY xyPoint =
    g
        [ class [ "point" ] ]
        [ TypedSvg.line
            [ x1 (Scale.convert scaleX xyPoint.childx)
            , y1 (Scale.convert scaleY xyPoint.childy)
            , x2 (Scale.convert scaleX xyPoint.parentx)
            , y2 (Scale.convert scaleY xyPoint.parenty)
            ]
            []
        ]

--for drawing circle and text for circle
point scaleX scaleY xyPoint =
    g
        [ class [ "point" ]
        , fontSize <| Px 18.0
        , fontFamily [ "serif" ]
        , transform
            [ Translate
                (Scale.convert scaleX xyPoint.childx)
                (Scale.convert scaleY xyPoint.childy)
            ]
        ]
        [ circle [ cx 0, cy 0, r (radius - 1.5) ] []
        , text_ [ x 0
                , y -10
                , textAnchor AnchorMiddle
                , transform 
                    [ Translate -5.5 -20.5
                    , Rotate 90.0 0.0 0.0
                    ] 
                ] 
                [ Html.text xyPoint.label ]
        ]

--general settings for visualization
w : Float
w =
    3000


h : Float
h =
    400


padding : Float
padding =
    30


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