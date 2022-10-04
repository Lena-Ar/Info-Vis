module Scatterplot exposing (..)

import Browser
import Csv exposing (parse)
import Csv.Decode exposing (..)
import Html exposing (Html, pre, text)
import Http
import Axis
import Scale exposing (ContinuousScale, domain)
import Statistics
import TypedSvg exposing (circle, g, line, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

type Msg
    = GotText (Result Http.Error String)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
{--
daten : List String
daten =
    [ "XBoxOne_GameSales_test.csv" ]
--}
init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "https://raw.githubusercontent.com/Lena-Ar/Info-Vis/main/Daten/CSV/XboxOne_GameSales_test.csv"
        , expect = Http.expectString GotText
        }
    )
{--
loadingGameSales : (Result Http.Error String -> Msg) -> Cmd Msg
loadingGameSales gotText = 
    daten
        |> List.map
            (\d ->
                Http.get
                    { url = "https://raw.githubusercontent.com/Lena-Ar/Info-Vis/main/Daten/CSV/" ++ d
                    , expect = Http.expectString gotText
                    }
            )
        |> Cmd.batch
--}
type alias GameSales =
    { game : String
    , genre : String
    , publisher : String
    , northAmerica : Float
    , europe : Float
    , japan : Float
    , restOfWorld : Float
    , global : Float
    }

type Model
  = Error
  | Loading
  | Success (List GameSales)

type alias Point =
    { pointGame : String
    , pointNorthAmerica : Float
    , pointEurope : Float
    , pointJapan : Float
    , pointRestOfWorld : Float
    , pointGlobal : Float
    }


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }

--Decoder
decodeGameSales : Csv.Decode.Decoder (GameSales -> a) a
decodeGameSales =
    Csv.Decode.map GameSales
        (Csv.Decode.field "Game" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "Genre" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Publisher" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "North America" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Europe" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Japan" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Rest of World" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Global" (String.toFloat >> Result.fromMaybe "error parsing string"))
        )

csvString_to_data : String -> List GameSales
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeGameSales
        |> Result.toMaybe
        |> Maybe.withDefault []

gamesSalesList :List String -> List GameSales
gamesSalesList listGame =
    List.map(\x -> csvString_to_data x) listGame
        |> List.concat

--mapping games to point---
--helpers for mapping because of lacking Maybe.map6--

{-- 
based on https://package.elm-lang.org/packages/elm/core/latest/Maybe#map2
; helper which helps with piping (which is a function)/applying map2 to be bigger one following
--}

map2pipe : Maybe a -> Maybe ( a -> b) -> Maybe b
map2pipe = 
    Maybe.map2 (|>)

{-- first try: basically just did map5 but without the helper map5
; instead piping; tried with too many arguments; not thought through 
helpMap : (a -> b -> c -> d -> e -> f) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f
helpMap apply m1 m2 m3 m4 m5 m6 = 
    Just apply
        |> m1
        |> m2
        |> m3
        |> m4
        |> m5
        |> m6
--}
{-- 
now: based on https://package.elm-lang.org/packages/elm/core/latest/Maybe#map5 but with one more to transform
; therefore map2pipe needed to handle one more than map5 would do
; applying map2pipe to transform all like a map6 would do if it existed
; with helper of map2 in map2pipe 
--}
helpMapBig : (nam -> na -> eu -> ja -> row -> gl) -> Maybe nam -> Maybe na -> Maybe eu -> Maybe ja -> Maybe row -> Maybe gl
helpMapBig apply a b c d e = 
    Just apply
        |> map2pipe a
        |> map2pipe b
        |> map2pipe c
        |> map2pipe d
        |> map2pipe e

-- based on https://ellie-app.com/hhZMpcRnTwFa1 (my code for exercise 1) --
assignment : GameSales -> Maybe Point
assignment game =
    helpMapBig
        (\northAmerica europe japan restOfWorld global ->
            Point
                (game.game ++ " (" ++ game.publisher ++ ")")
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

-- filtering games --
-- based on https://ellie-app.com/hhZMpcRnTwFa1 (my code for exercise 1) --
filterAndReduceGames : List GameSales -> XyData
filterAndReduceGames games =
    let
        filter =
            List.filterMap assignment games
    in
    XyData "North America" "Europe" filter



--cases for buttons to be added
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| gamesSalesList [fullText], Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



--view to be coded

view : Model -> Html Msg
view model =
    case model of
        Error ->
            text "Opening the data for sales of games on XBoxOne failed"

        Loading ->
            text "Loading GameSales data..."
        
        Success fullText ->
            let 
                gameSalesData: List GameSales
                gameSalesData = 
                    fullText

                number_games: Int
                number_games =
                    List.length gameSalesData
                
                xy_games = 
                    filterAndReduceGames fullText
            
            in
            Html.div [] 
                [Html.p []
                    [ Html.text (String.fromInt number_games) ]
                , scatterplot xy_games
                ]

----------point--------------
---test with North America on x-Axis and Europe on y-Axis, description is name of game---

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY xyPoint =
    g
        [ class [ "point" ]
        , fontSize <| Px 10.0
        , fontFamily [ "sans-serif" ]

        --Positionierung der Punkte
        , transform
            [ Translate
                (Scale.convert scaleX xyPoint.pointNorthAmerica)
                (Scale.convert scaleY xyPoint.pointEurope)
            ]
        ]
        {--Vom Testpunkt kopiert, nur Text geändert und Radius der Kreise zur 
        besseren Sichtbarkeit/Unterscheidung der Punkte--}
        [ circle [ cx 0, cy 0, r 4 ] []
        , text_
            [ x 0, y -15, TypedSvg.Attributes.textAnchor AnchorMiddle ]
            [ TypedSvg.Core.text xyPoint.pointGame ]
        ]



----Scatterplot--------------------
------------------------------------
scatterplot : XyData -> Svg msg
scatterplot model =
    let
        --x-Werte/NorthAmerica
        xValues : List Float
        xValues =
            List.map .pointNorthAmerica model.data

        --y-Werte/Europe
        yValues : List Float
        yValues =
            List.map .pointEurope model.data

        --Abbildungen/Umrechnungen auf SVG
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
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; fill: rgb(18, 132, 90)}
            """ ]

        , g
            [ transform [ Translate (padding - 1) (padding - 1) ]
            , class [ "point" ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            []
        --x-Achse
        , g
            [ transform [ Translate padding (h - padding) ] ]
            [ xAxis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPositions.x + 25)
                , y 35
                , TypedSvg.Attributes.textAnchor AnchorMiddle
                , fontSize <| Px 17.0
                , fontFamily [ "sans-serif" ]
                ]
                [ Html.text model.xDescription ]
            ]

        -- y-Achse
        , g
            [ transform [ Translate padding padding ] ]
            [ yAxis yValues
            , text_
                [ x 0
                , y (Scale.convert yScaleLocal labelPositions.y - 15)
                , TypedSvg.Attributes.textAnchor AnchorMiddle
                , fontSize <| Px 17.0
                , fontFamily [ "sans-serif" ]
                ]
                [ Html.text model.yDescription ]
            ]

        --SVG der Points
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        ]



---------------------------------------------
------------general settings for scatterplot----------


w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


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