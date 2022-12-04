module ParallelPlot exposing (..)

import Browser
import Csv exposing (parse)
import Csv.Decode exposing (..)
import Html exposing (Html, pre, text)
import Html.Attributes exposing (href, placeholder, type_, value)
import Html.Events exposing (..)
import Http
import Statistics
import Axis
import Shape
import Scale exposing (ContinuousScale, domain)
import List.Extra
import Color
import Path
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox, fill)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Html exposing (button)
import Data exposing (MultiDimData, MultiDimPoint, GameSales, RegionType, regionTypeToAxisAnnotation)


-- mapping and filtering for Null-values --

-- two helper functions to work around lacking Maybe.map6 --
-- the same as in Scatterplot --

-- idea based on https://package.elm-lang.org/packages/elm/core/latest/Maybe#map2 --
{-- 
    helper to help piping/applying Maybe.map2 to helpMapBig 
--}
map2pipe : Maybe a -> Maybe ( a -> b) -> Maybe b
map2pipe = 
    Maybe.map2 (|>)


-- idea based on https://package.elm-lang.org/packages/elm/core/latest/Maybe#map5 but with one more to transform --
{-- 
    therefore map2pipe needed to handle one more than map5 would do
    applying map2pipe to transform all like a map6 would do if it existed
    with helper of map2 in map2pipe 
--}
helpMapBig : (nam -> na -> eu -> ja -> row -> gl) -> Maybe nam -> Maybe na -> Maybe eu -> Maybe ja -> Maybe row -> Maybe gl
helpMapBig apply a b c d e = 
    Just apply
        |> map2pipe a
        |> map2pipe b
        |> map2pipe c
        |> map2pipe d
        |> map2pipe e


-- filtering and assigning games with help of the two above helpers --
-- based on code for exercise 6 (https://ellie-app.com/hCYJdyzzB7wa1) --
{-- 
    basically the same as in Scatterplot, but with rewrite of assignment and reducement because of no need for XyData here
--}
assignmentAndReduce : List GameSales -> List GameSales
assignmentAndReduce game =
    let
       assignment : GameSales -> Maybe GameSales
       assignment assign = 
            helpMapBig
                (GameSales assign.game assign.genre assign.publisher )
                (Just assign.northAmerica)
                (Just assign.europe)
                (Just assign.japan)
                (Just assign.restOfWorld)
                (Just assign.global) 
    in
    List.filterMap assignment game


-- used for free selection of axes and possibility for application of genre filter --
-- applied in multiDimFunction which gives this functions the needed data --
-- loosely based on code for exercise 6 (https://ellie-app.com/hCYJdyzzB7wa1) --
multiDimenData : List GameSales -> RegionType -> RegionType -> RegionType -> RegionType -> RegionType -> (GameSales -> String) -> (GameSales -> String) -> String -> String -> String -> String -> String -> MultiDimData
multiDimenData game a1 a2 a3 a4 a5 name pub na1 na2 na3 na4 na5=
    MultiDimData [na1, na2, na3, na4, na5]
    --[Data.regionTypeToString Data.NorthAmerica, Data.regionTypeToString Data.Europe, Data.regionTypeToString Data.Japan, Data.regionTypeToString Data.RestOfWorld, Data.regionTypeToString Data.Global]
        [ List.map
            (\data ->
                -- conversion needed in order to use RegionType as Input to convert to (GameSales -> Float) to get Floats for MultiDimData --
                [  (regionTypeToAxisAnnotation a1 data) , (regionTypeToAxisAnnotation a2 data), (regionTypeToAxisAnnotation a3 data), (regionTypeToAxisAnnotation a4 data), (regionTypeToAxisAnnotation a5 data) ]
                    |> MultiDimPoint (name data) (pub data)
            )
            game
        ]


-- plots parallel coordinates plot --
-- based on exercise 6.1 (https://ellie-app.com/hCYJdyzzB7wa1) --
scatterplotParallel : String -> Float -> Float -> MultiDimData -> Svg msg
scatterplotParallel css w ar model =
    let
        h : Float
        h =
            w / ar

        -- positioning of axes in x-direction --
        xScale =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )

        transformListe : List (List Float)
        transformListe =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        wideExtentListe : List ( Float, Float )
        wideExtentListe =
            transformListe |> List.map wideExtent

        -- list of ScaleLinear of each dimension --
        scaleListe =
            List.map (Scale.linear ( h, 0 )) wideExtentListe

        -- list of axes left for each dimension --
        axisListe =
            List.map (Axis.left [ Axis.tickCount tickCount ]) scaleListe
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100
        ]
    <|
        [ style [] 
            [TypedSvg.Core.text css]

        -- surrounding rectangle --
        , TypedSvg.rect
            [ TypedSvg.Attributes.x1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.y1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.width <| TypedSvg.Types.Px (w + 2 * padding - 1)
            , TypedSvg.Attributes.height <| TypedSvg.Types.Px (h + 2 * padding - 1)
            , TypedSvg.Attributes.fill <| Paint <| Color.white
            , stroke <| Paint <| Color.white
            , strokeWidth <| Px 0.5
            ]
            []

        -- positioning of axes --
        , g [ TypedSvg.Attributes.class [ "paralleleAchse" ] ]
            [ g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\index axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xScale (toFloat index + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    axisListe

            -- positioning of description --
            , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\index beschreibung ->
                        text_
                            [ fontFamily [ "sans-serif" ]
                            , fontSize (Px 10)
                            , x <| Scale.convert xScale (toFloat index + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text beschreibung ]
                    )
                    model.dimDescription
            ]
        ]
            -- drawing of points with Shape.linearCurve --
            ++ (let
                    point p game publisher descript =
                        let
                            graphline : Path.Path
                            graphline =
                                List.map3
                                    (\description s px ->
                                        Just
                                            ( Scale.convert xScale <| toFloat description
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    scaleListe
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        g [class ["cssparallel"]]
                            [
                            Path.element graphline
                                [ stroke <| Paint <| Color.black
                                , strokeWidth <| Px 0.9
                                , fill PaintNone
                                , class ["cssparallel"]
                                ]
                                , text_ 
                                [ x 300
                                , y -30
                                , fontSize (Px 10.5)
                                , TypedSvg.Attributes.textAnchor AnchorMiddle
                                ]
                                [TypedSvg.Core.text 
                                    (game ++ ", publisher: " ++ publisher ++ 
                                        (String.concat <|(List.map2(\a b -> ", " ++ a ++ ": " ++ (String.fromFloat b) ++ " ") descript p))
                                    )
                                ]
                            ]
                in
                model.data
                    |> List.map
                        (\database ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (\descr -> point descr.value descr.pointName descr.pointPublisher model.dimDescription) database)
                        )
               )

-- globally defined CSS --
-- normal opacity not 1 to get x-ray effect with white surrounding rectangle --
cssParallel : String
cssParallel = 
    """
        .cssparallel { stroke: rgba(46, 78, 23, 0.8); opacity: 0.5 }
        .cssparallel text { display: none; }
        .cssparallel:hover { stroke: rgb(75, 128, 36,1); stroke-width: 1.7; opacity: 1}
        .cssparallel:hover text { display: inline; stroke: rgba(255, 255, 255, 1); stroke-width: 0.03; fill: rgb(75, 128, 36, 0.8)}
    """ 


-- general definitions for scatterplotParallel --
padding : Float
padding =
    50


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )