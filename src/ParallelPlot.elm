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
{-}
daten : List String
daten =
    [ "XBoxOne_GameSales_test" ]
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
loadingGameSales game = 
    daten
        |> List.map
            (\d ->
                Http.get
                    { url = "https://raw.githubusercontent.com/Lena-Ar/Info-Vis/main/Daten/CSV/" ++ d
                    , expect = Http.expectString GotText
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

-- from Scatterplot
type alias MultiPoint =
    { pointGenre : String
    , pointNorthAmerica : Float
    , pointEurope : Float
    , pointJapan : Float
    , pointRestOfWorld : Float
    , pointGlobal : Float
    }
--from early version of scatterplot
--cases for buttons to be added
type Model
  = Error
  | Loading
  | Success 
    { data: List GameSales
    }

--exercise 6.1
type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }

--from scatterplot
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

--cases for buttons to be added
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = gamesSalesList [fullText] }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

--assignment from Scatterplot--
map2pipe : Maybe a -> Maybe ( a -> b) -> Maybe b
map2pipe = 
    Maybe.map2 (|>)

helpMapBig : (nam -> na -> eu -> ja -> row -> gl) -> Maybe nam -> Maybe na -> Maybe eu -> Maybe ja -> Maybe row -> Maybe gl
helpMapBig apply a b c d e = 
    Just apply
        |> map2pipe a
        |> map2pipe b
        |> map2pipe c
        |> map2pipe d
        |> map2pipe e

-- based on https://ellie-app.com/hCYJdyzzB7wa1 (my code for exercise 6) --
-- basically the same as in Scatterplot, but with rewrite of assignment and reducement because of no XyData here--
assignmentAndReduce : List GameSales -> List MultiPoint
assignmentAndReduce game =
    let
       assignment : GameSales -> Maybe MultiPoint
       assignment assign = 
            helpMapBig
                (\northAmerica europe japan restOfWorld global ->
                    MultiPoint
                        (assign.game ++ " , " ++  assign.publisher ++ " ")
                        (northAmerica)
                        (europe)
                        (japan)
                        (restOfWorld)
                        (global)
                )
                (Just assign.northAmerica)
                (Just assign.europe)
                (Just assign.japan)
                (Just assign.restOfWorld)
                (Just assign.global) 
    in
    List.filterMap assignment game

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
                    fullText.data
                    
                number_games: Int
                number_games =
                    List.length gameSalesData
                
                number_games_genre: Int
                number_games_genre =  
                    List.length filteredGamesGenre
                
                --from exercise 6.1
                filteredGamesGenre : List MultiPoint
                filteredGamesGenre =
                    assignmentAndReduce fullText.data
                        |> List.filter
                        (.pointGenre >> (==) "Action")

                multiDimenData =
                    MultiDimData [ "North America", "Europe", "Japan", "Rest of World", "Global" ]
                        [ List.map
                            (\data ->
                                [ data.pointNorthAmerica, data.pointEurope, data.pointJapan, data.pointRestOfWorld, data.pointGlobal ]
                                    |> MultiDimPoint data.pointGenre
                            )
                            filteredGamesGenre
                        ]

            in
            Html.div [] 
                [Html.text ("Number of games: " ++ String.fromInt number_games)
                , Html.br [] []
                , Html.text ("Number of games in selected genre: " ++ String.fromInt number_games_genre)
                , scatterplotParallel 600 2 multiDimenData
                ]



-- plot based on exercise 6.1--
scatterplotParallel : Float -> Float -> MultiDimData -> Svg msg
scatterplotParallel w ar model =
    let
        h : Float
        h =
            w / ar

        --Positionierung der Achsen in x-Richtung
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

        --Listen ScaleLinear jeder Dimension
        scaleListe =
            List.map (Scale.linear ( h, 0 )) wideExtentListe

        --Liste der Axis left für jede Dimension
        axisListe =
            List.map (Axis.left [ Axis.tickCount tickCount ]) scaleListe
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100
        ]
    <|
        [ TypedSvg.style []
            []

        --Umgebungsrechteck
        , TypedSvg.rect
            [ TypedSvg.Attributes.x1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.y1 <| TypedSvg.Types.Px 1
            , TypedSvg.Attributes.width <| TypedSvg.Types.Px (w + 2 * padding - 1)
            , TypedSvg.Attributes.height <| TypedSvg.Types.Px (h + 2 * padding - 1)
            , TypedSvg.Attributes.fill <| Paint <| Color.white
            , stroke <| Paint <| Color.grey
            , strokeWidth <| Px 0.5
            ]
            []

        --Achsenpositionierung
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

            --Beschreibungspositionierung
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
            --Zeichnung der Punkte angelehnt an Ü5 mit Shape.linearCurve
            ++ (let
                    punkt p =
                        let
                            graphenlinie : Path.Path
                            graphenlinie =
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
                        Path.element graphenlinie
                            [ stroke <| Paint <| Color.black
                            , strokeWidth <| Px 0.5
                            , fill PaintNone
                            ]
                in
                model.data
                    |> List.map
                        (\datensatz ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (.value >> punkt) datensatz)
                        )
               )

--general settings for plot--
--from exercise 6.1--
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