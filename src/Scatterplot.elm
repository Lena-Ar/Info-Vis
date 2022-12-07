module Scatterplot exposing (..)

import Html exposing (text)
import Axis
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Data exposing (Point, XyData, RegionType(..), GameSales)


-- mapping and filtering for Null-values --

-- two helper functions to work around lacking Maybe.map6 --
-- the same as in ParallelPlot --
 
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
-- based on code for exercise 1 (https://ellie-app.com/hhZMpcRnTwFa1) --
filterAndReduceGames : List GameSales -> XyData
filterAndReduceGames games =
    let
        assignment : GameSales -> Maybe Point
        assignment game =
            helpMapBig
                (\northAmerica europe japan restOfWorld global ->
                    Point
                        (game.game ++ " , " ++  game.publisher ++ " ")
                        (northAmerica)
                        (europe)
                        (japan)
                        (restOfWorld)
                        (global)
                )
                (Just game.northAmerica)
                (Just game.europe)
                (Just game.japan)
                (Just game.restOfWorld)
                (Just game.global)
        filter =
            List.filterMap assignment games
    in
    XyData "North America" "Europe" filter
      

-- needed for free selection of x- and y-axis --
-- applied with giving data in valuesX and valuesY in case for Scatterplot in MainScatterParallel --
regionFilter : List GameSales -> RegionType -> List Float
regionFilter points regionType =
    case regionType of
        NorthAmerica ->
            List.map .northAmerica points

        Europe ->
            List.map .europe points

        Japan ->
            List.map .japan points

        RestOfWorld ->
            List.map .restOfWorld points
                    
        Global -> 
            List.map .global points


-- definition of point for use in scatterplot --
point : ContinuousScale Float -> ContinuousScale Float -> Point -> (Float, Float) -> Svg msg
point scaleX scaleY pointLabel xyPoint =
    g
        [ class [ "point" ]
        , fontSize <| Px 9.0
        , fontFamily [ "sans-serif" ]
        ]
            [circle
            --Positionierung der Punkte
                [cx <| Scale.convert scaleX <| Tuple.first xyPoint
                , cy <| Scale.convert scaleY <| Tuple.second xyPoint
                , r radius
                ]
                []
            ,TypedSvg.text_
                [ x <| Scale.convert scaleX <| Tuple.first xyPoint
                , y <| (Scale.convert scaleY <| Tuple.second xyPoint) - (radius + 1.5)
                , textAnchor AnchorMiddle 
                ]
            [ text <| 
                pointLabel.pointGame
                    ++ "("
                    ++ (String.fromFloat <| Tuple.first xyPoint)
                    ++ " , "
                    ++ (String.fromFloat <| Tuple.second xyPoint)
                    ++ ")" 
            ]
            ]


-- Scatterplot to draw plot--
scatterplot : String -> XyData -> List Float -> List Float -> String -> String -> Svg msg
scatterplot css model xValues yValues labelX labelY =
    let
        -- to ensure variability of axes/x- and y-values --
        pointsXY = 
            List.map2 (\x y -> ( x, y )) xValues yValues

        -- compute to SVG --
        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text css]
        , g
            [ transform [ Translate (padding - 1) (padding - 1) ]
            , class [ "point" ]
            , fontSize <| Px 9.0
            , fontFamily [ "sans-serif" ]
            ]
            []
        -- x-axis --
        , g
            [ transform [ Translate padding (h - padding) ] ]
            [ xAxis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPositions.x + 25)
                , y 40
                , TypedSvg.Attributes.textAnchor AnchorMiddle
                , fontSize <| Px 13
                , fontFamily [ "sans-serif" ]
                ]
                [ text labelX ]
            ]

        -- y-axis --
        , g
            [ transform [ Translate padding padding ] ]
            [ yAxis yValues
            , text_
                [ x 0
                , y (Scale.convert yScaleLocal labelPositions.y - 15)
                , TypedSvg.Attributes.textAnchor AnchorMiddle
                , fontSize <| Px 13
                , fontFamily [ "sans-serif" ]
                ]
                [ text labelY ]
            ]

        -- points --
        , g [ transform [ Translate padding padding ] ]
            (List.map2 (point xScaleLocal yScaleLocal) model.data pointsXY)
        ]


-- globally defined CSS --
cssPoint : String
cssPoint = 
    """
        .point circle { stroke: rgba(46, 78, 23, 0.8); fill: rgba(255, 255, 255,0.3); }
        .point text { display: none; }
        .point:hover circle { stroke: rgba(255, 255, 255, 1); fill: rgb(75, 128, 36,0.8); }
        .point:hover text { display: inline; stroke: rgba(255, 255, 255, 1); stroke-width: 0.03; fill: rgb(75, 128, 36, 0.8)}
    """ 


-- general definitions for scatterplot--
w : Float
w =
    800


h : Float
h =
    500


padding : Float
padding =
    65

radius : Float
radius =
    4.0


tickCount : Int
tickCount =
    10


xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)


xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) (wideExtent values)


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) (wideExtent values)


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


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )