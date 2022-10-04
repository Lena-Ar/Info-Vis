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

--cases for buttons to be added
type Model
  = Error
  | Loading
  | Success (List String)

--Decoder
decodeGameSales : Csv.Decode.Decoder ((String, Float) -> a) a
decodeGameSales =
    Csv.Decode.map (\a b -> (a, b))
        (Csv.Decode.field "Game" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "Japan" (String.toFloat >> Result.fromMaybe "error parsing string"))
        )

csvString_to_data : String -> List (String, Float)
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeGameSales
        |> Result.toMaybe
        |> Maybe.withDefault []

gamesSalesList :List (String, Float) -> Html Msg
gamesSalesList listGame =
    Html.ul []
        (List.map (\( a, b ) -> Html.li [] [ text <| a ++ ", " ++ (String.fromFloat b) ]) listGame)

--cases for buttons to be added
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        liste =
            case model of
                Success list ->
                    list

                Error ->
                    []

                Loading ->
                    []
    in
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| liste ++ [ fullText ], Cmd.none )

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
        
        Success list ->
            Html.div [] <|
                List.map (\fulltext -> pre [] [ gamesSalesList  <| csvString_to_data fulltext ]) list

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