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
--same concept as in Scatterplot
type Msg
    = GotText (Result Http.Error String)
    | ChangeGenreType String

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
    { pointGame : String
    , pointPublisher : String
    , pointGenre : String
    , pointNorthAmerica : Float
    , pointEurope : Float
    , pointJapan : Float
    , pointRestOfWorld : Float
    , pointGlobal : Float
    }
--from early version of scatterplot
--same concept as in Scatterplot
type Model
  = Error
  | Loading
  | Success 
    { data: List GameSales
    , genre: String
    }

--exercise 6.1
type alias MultiDimPoint =
    { pointName : String
    , pointPublisher : String
    , value : List Float }


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

--filter and button from Scatterplot
filterGenre : List GameSales -> String -> List GameSales
filterGenre allGames genretype =
    List.filter (\c -> c.genre == genretype) allGames

buttonGenreType : Html Msg
buttonGenreType =
    Html.select
        [ onInput ChangeGenreType ]
        [ Html.option [ value "Action" ] [ Html.text "Action" ]
        , Html.option [ value "Action-Adventure" ] [ Html.text "Action-Adventure" ]
        , Html.option [ value "Adventure" ] [ Html.text "Adventure" ]
        , Html.option [ value "Fighting" ] [ Html.text "Fighting" ]
        , Html.option [ value "Misc" ] [ Html.text "Misc" ]
        , Html.option [ value "MMO" ] [ Html.text "MMO" ]
        , Html.option [ value "Music" ] [ Html.text "Music" ]
        , Html.option [ value "Platform" ] [ Html.text "Platform" ]
        , Html.option [ value "Puzzle" ] [ Html.text "Puzzle" ]
        , Html.option [ value "Racing" ] [ Html.text "Racing" ] 
        , Html.option [ value "Role-Playing" ] [ Html.text "Role-Playing" ] 
        , Html.option [ value "Shooter" ] [ Html.text "Shooter" ] 
        , Html.option [ value "Simulation" ] [ Html.text "Simulation" ] 
        , Html.option [ value "Sports" ] [ Html.text "Sports" ]
        , Html.option [ value "Strategy" ] [ Html.text "Strategy" ]
        ]

--cases for buttons to be added
--ChangeGenreType same concept as in Scatterplot
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = gamesSalesList [fullText], genre = "Action" }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        
        ChangeGenreType new_genre -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = new_genre }, Cmd.none ) 
                _ ->
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
                (MultiPoint assign.game assign.publisher assign.genre)
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
                        (.pointGenre >> (==) fullText.genre)

                multiDimenData =
                    MultiDimData [ "North America", "Europe", "Japan", "Rest of World", "Global" ]
                        [ List.map
                            (\data ->
                                [ data.pointNorthAmerica, data.pointEurope, data.pointJapan, data.pointRestOfWorld, data.pointGlobal ]
                                    |> MultiDimPoint data.pointGame data.pointPublisher
                            )
                            filteredGamesGenre
                        ]

            in
            Html.div [Html.Attributes.style "padding" "10px"]
                [ Html.h1 [Html.Attributes.style "fontSize" "30px"] 
                    [ Html.text ("Parallel Coordinates Plot of Video Game Sales for XBox One") ]
                , Html.h2 [Html.Attributes.style "fontSize" "20px"] 
                --to be specified and explained more
                    [ Html.text ("This parallel coordinates plot shows the sales of video games in millions of units for XBox One sorted by selected genre.") ]
                , Html.p [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Number of all games across all genres: " ++ String.fromInt number_games)]
                , Html.h4 [Html.Attributes.style "fontSize" "16px"]
                    [ Html.text ("Please choose the genre you want to display with the button below.") ]
                , Html.p [Html.Attributes.style "padding" "10px"]
                    [ buttonGenreType ]
                , Html.p [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Number of games in selected genre: " ++ String.fromInt number_games_genre)]
                , Html.h2 [Html.Attributes.style "fontSize" "20px"]
                    [Html.text ("Parallel Coordinates Plot for " ++ fullText.genre )]
                , scatterplotParallel cssParallel 600 2 multiDimenData
                ]



-- plot based on exercise 6.1--
scatterplotParallel : String -> Float -> Float -> MultiDimData -> Svg msg
scatterplotParallel css w ar model =
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
        [ style [] 
            [TypedSvg.Core.text css]

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
                    punkt p game publisher descript =
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
                        g [class ["cssparallel"]]
                            [
                            Path.element graphenlinie
                                [ stroke <| Paint <| Color.black
                                , strokeWidth <| Px 0.9
                                , fill PaintNone
                                , class ["cssparallel"]
                                ]
                                , text_ 
                                [ x 280
                                , y -30
                                , fontSize (Px 12)
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
                        (\datensatz ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (\descr -> punkt descr.value descr.pointName descr.pointPublisher model.dimDescription) datensatz)
                        )
               )

--opacity not 1 in normal to get the same effect as with x-ray but still white rectangle
cssParallel : String
cssParallel = 
    """
        .cssparallel { stroke: rgba(46, 78, 23, 0.8); opacity: 0.7 }
        .cssparallel text { display: none; }
        .cssparallel:hover { stroke: rgb(75, 128, 36,1); stroke-width: 1.7; opacity: 1}
        .cssparallel:hover text { display: inline; stroke: rgba(255, 255, 255, 1); stroke-width: 0.03; fill: rgb(75, 128, 36, 0.8)}
    """ 


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