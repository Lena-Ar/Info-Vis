module Scatterplot exposing (..)

import Browser
import Csv exposing (parse)
import Csv.Decode exposing (..)
import Html exposing (Html, pre, text, br)
import Html.Events exposing (onInput)
import Html.Attributes exposing (value)
import Http
import Axis
import Scale exposing (ContinuousScale, domain)
import Statistics
import TypedSvg exposing (circle, g, line, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Data exposing (Point, XyData, RegionType, GameSales)
{--
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
    | ChangeGenreType String
    | ChangeRegionX RegionType
    | ChangeRegionY RegionType


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none--}
{--
daten : List String
daten =
    [ "XBoxOne_GameSales_test.csv" ]
--}{--
init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "https://raw.githubusercontent.com/Lena-Ar/Info-Vis/main/Daten/CSV/XboxOne_GameSales_test.csv"
        , expect = Http.expectString GotText
        }
    )--}
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
--}{--
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
--}{--
type Model
  = Error
  | Loading
  | Success 
    { data: List Data.GameSales
    , genre: String
    , xaxis: RegionType
    , yaxis: RegionType
    --, genre: GenreType
    }--}
{--
type GenreType
    = Action
    | Action_Adventure
    | Adventure
    | Fighting
    | Misc
    | MMO
    | Music
    | Platform
    | Puzzle
    | Racing
    | Role_Playing
    | Shooter
    | Simulation
    | Sports
    | Strategy
--}

--maybe error bc of field from decoding -> maybe change csv?
{--
type RegionType
    = NorthAmerica
    | Europe
    | Japan
    | RestOfWorld
    | Global
--}
{--
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
    --}
{--
--Decoder
decodeGameSales : Csv.Decode.Decoder (Data.GameSales -> a) a
decodeGameSales =
    Csv.Decode.map Data.GameSales
        (Csv.Decode.field "Game" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "Genre" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "Publisher" Ok)
            |> Csv.Decode.andMap (Csv.Decode.field "North America" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Europe" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Japan" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Rest of World" (String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "Global" (String.toFloat >> Result.fromMaybe "error parsing string"))
        )

csvString_to_data : String -> List Data.GameSales
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeGameSales
        |> Result.toMaybe
        |> Maybe.withDefault []

gamesSalesList :List String -> List Data.GameSales
gamesSalesList listGame =
    List.map(\x -> csvString_to_data x) listGame
        |> List.concat
--}
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
--maybe need changes here--
{--
assignment : Data.GameSales -> Maybe Point
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
        (Just game.global)--}

-- filtering &assigning games --
-- based on https://ellie-app.com/hhZMpcRnTwFa1 (my code for exercise 1) --
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

--filter for genre--
{--
filterGenre : List Data.GameSales -> String -> List Data.GameSales
filterGenre allGames genretype =
    List.filter (\c -> c.genre == genretype) allGames
    --}
{--
--genre ändern
genreTypeToString : GenreType -> String
genreTypeToString genreType =
    case genreType of
        Action -> 
            "Action"

        Action_Adventure ->
            "Action-Adventure"

        Adventure ->
            "Adventure"

        Fighting ->
            "Fighting"

        Misc ->
            "Misc"

        MMO ->
            "MMO"

        Music ->
            "Music"
        
        Platform ->
            "Platform"
        
        Puzzle ->
            "Puzzle"
        
        Racing ->
            "Racing"
        
        Role_Playing ->
            "Role-Playing"
        
        Shooter ->
            "Shooter"
        
        Simulation ->
            "Simulation"
        
        Sports ->
            "Sports"
        
        Strategy ->
            "Strategy"

stringToGenreType : String -> GenreType
stringToGenreType stringGenreType =
    if stringGenreType == "Action" then
        Action

    else if stringGenreType == "Action-Adventure" then
        Action_Adventure

    else if stringGenreType == "Adventure" then
        Adventure

    else if stringGenreType == "Fighting" then
        Fighting

    else if stringGenreType == "Misc" then
        Misc

    else if stringGenreType == "MMO" then
        MMO

    else if stringGenreType == "Music" then
        Music

    else if stringGenreType == "Platform" then
        Platform

    else if stringGenreType == "Puzzle" then
        Puzzle
    
    else if stringGenreType == "Racing" then
        Racing
    
    else if stringGenreType == "Role-Playing" then
        Role_Playing
    
    else if stringGenreType == "Shooter" then
        Shooter
    
    else if stringGenreType == "Simulation" then
        Simulation

    else if stringGenreType == "Sports" then
        Sports
    
    else
        Strategy
--}
{--
--attributType/region
regionTypeToString : RegionType -> String
regionTypeToString regionType =
    case regionType of
        NorthAmerica -> 
            "North America"

        Europe ->
            "Europe"

        Japan ->
            "Japan"

        RestOfWorld ->
            "Rest of world"

        Global ->
            "Global"

        
stringToRegionType : String -> RegionType
stringToRegionType stringRegionType =
    if stringRegionType == "North America" then
        NorthAmerica

    else if stringRegionType == "Europe" then
        Europe

    else if stringRegionType == "Japan" then
        Japan

    else if stringRegionType == "Rest of world" then
        RestOfWorld

    else
        Global
--}
{--
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
--}
{--
buttonRegionTypeX : Html Msg
buttonRegionTypeX =
    Html.select
        [ onInput (\rx -> stringToRegionType rx |> ChangeRegionX) ]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonRegionTypeY : Html Msg
buttonRegionTypeY =
    Html.select
        [ onInput (\ry -> stringToRegionType ry |> ChangeRegionY) ]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

--cases for buttons to be added
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                --to be included with regions according to model
                    ( Success <| { data = Data.gamesSalesList [fullText], genre = "Action", xaxis = NorthAmerica , yaxis = NorthAmerica}, Cmd.none )
                Err _ ->
                    (model, Cmd.none)

        ChangeGenreType new_genre -> 
                --to be included with regions according to model
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = new_genre, xaxis = a.xaxis, yaxis = a.yaxis }, Cmd.none ) 
                _ ->
                    ( model, Cmd.none )
        
        ChangeRegionX new_regionx -> 
            case model of
                Success a -> 
                    (Success <| { data = a.data, genre = a.genre, xaxis = new_regionx, yaxis = a.yaxis}, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeRegionY new_regiony ->
            case model of
                Success a -> 
                    (Success <| { data = a.data, genre = a.genre, xaxis = a.xaxis, yaxis = new_regiony}, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
--}
{--
--to be adjusted for axisChange/region--
view : Model -> Html Msg
view model =
    case model of
        Error ->
            text "Opening the data for sales of games on XBoxOne failed"

        Loading ->
            text "Loading GameSales data..."
        
        Success fullText ->
            Html.div [Html.Attributes.style "padding" "10px"]
                [ Html.h1 [Html.Attributes.style "fontSize" "30px"] 
                    [ Html.text ("Scatterplot of Video Game Sales for XBox One") ]
                , Html.h2 [Html.Attributes.style "fontSize" "20px"] 
                    [ Html.text ("This scatterplot shows the sales of video games in millions of units for XBox One sorted by selected genre.") ]
                , Html.p [Html.Attributes.style "fontSize" "17px"]
                    [Html.text ("Hint: The x-axis and y-axis can be adjusted to your needs by seleceting the needed regions for each axis with the buttons below.")]
                , Html.p [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Number of all games across all genres: " ++ String.fromInt number_games)
                    , Html.br [] []
                    , Html.text ("Number of all games across all genres cleared by potential Null-values: " ++ String.fromInt number_clearedGames)
                    ]
                , Html.h4 [Html.Attributes.style "fontSize" "16px"]
                    [ Html.text ("Please choose the genre you want to display with the button below.") ]
                , Html.p [Html.Attributes.style "padding" "10px"]
                    [ buttonGenreType ]
                , Html.p [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Number of games in selected genre: " ++ String.fromInt number_filteredGames) ]
                , Html.h4 [Html.Attributes.style "fontSize" "16px"]
                    [ Html.text ("Please choose the region you want to display on the xaxis first (on the left) and the one you want to display on the yaxis second (on the right).")]
                , Html.p [Html.Attributes.style "padding" "10px"]
                    [ buttonRegionTypeX
                    , buttonRegionTypeY ]
                , Html.h2 [Html.Attributes.style "fontSize" "20px"]
                    [Html.text ("Scatterplot for " ++ fullText.genre ++ " with " ++ (regionTypeToString fullText.xaxis) ++ " as x-axis and " ++ (regionTypeToString fullText.yaxis) ++ " as y-axis.")]
                , scatterplot cssPoint gameSalesDataCleared valuesX valuesY (regionTypeToString fullText.xaxis) (regionTypeToString fullText.yaxis)
                ]--}
                {--
gameSalesData: List Data.GameSales
gameSalesData = 
    fullText.data
                
gameSalesDataFiltered = 
    filterGenre fullText.data fullText.genre
                
                
gameSalesDataNull =
    filterAndReduceGames (fullText.data)
            
number_clearedGames: Int
number_clearedGames = 
    List.length gameSalesDataNull.data

number_games: Int
number_games =
    List.length gameSalesData

number_filteredGames : Int
number_filteredGames =
    List.length gameSalesDataFiltered
                
gameSalesDataCleared = 
    filterAndReduceGames (gameSalesDataFiltered)
          --}      
regionFilter : List GameSales -> RegionType -> List Float
regionFilter points regionType =
    case regionType of
        Data.NorthAmerica ->
            List.map .northAmerica points

        Data.Europe ->
            List.map .europe points

        Data.Japan ->
            List.map .japan points

        Data.RestOfWorld ->
            List.map .restOfWorld points
                    
        Data.Global -> 
            List.map .global points
{--         
valuesX : List Float
valuesX = 
    regionFilter gameSalesDataFiltered fullText.xaxis


valuesY : List Float
valuesY = 
    regionFilter gameSalesDataFiltered fullText.yaxis--}
----------point--------------

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
            [ Html.text <| 
                pointLabel.pointGame
                    ++ "("
                    ++ (String.fromFloat <| Tuple.first xyPoint)
                    ++ " , "
                    ++ (String.fromFloat <| Tuple.second xyPoint)
                    ++ ")" 
            ]
            ]
        

----Scatterplot--------------------
------------------------------------
scatterplot : String -> XyData -> List Float -> List Float -> String -> String -> Svg msg
scatterplot css model xValues yValues labelX labelY =
    let
        pointsXY = 
            List.map2 (\x y -> ( x, y )) xValues yValues

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
        [ style [] [ TypedSvg.Core.text css]
        , g
            [ transform [ Translate (padding - 1) (padding - 1) ]
            , class [ "point" ]
            , fontSize <| Px 9.0
            , fontFamily [ "sans-serif" ]
            ]
            []
        --x-Achse
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
                [ Html.text labelX ]
            ]

        -- y-Achse
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
                [ Html.text labelY ]
            ]

        --SVG der Points
        , g [ transform [ Translate padding padding ] ]
            (List.map2 (point xScaleLocal yScaleLocal) model.data pointsXY)
        ]

cssPoint : String
cssPoint = 
    """
        .point circle { stroke: rgba(46, 78, 23, 0.8); fill: rgba(255, 255, 255,0.3); }
        .point text { display: none; }
        .point:hover circle { stroke: rgba(255, 255, 255, 1); fill: rgb(75, 128, 36,0.8); }
        .point:hover text { display: inline; stroke: rgba(255, 255, 255, 1); stroke-width: 0.03; fill: rgb(75, 128, 36, 0.8)}
    """ 

---------------------------------------------
------------general settings for scatterplot----------


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