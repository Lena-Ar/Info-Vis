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
{--
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
--same concept as in Scatterplot
--now based on type annotations for MultiDimPoint as we have Floats and String in there
type Msg
    = GotText (Result Http.Error String)
    | ChangeGenreType String
    | ChangeFirstAxis (Data.GameSales -> Float, String)
    | ChangeSecondAxis (Data.GameSales -> Float, String)
    | ChangeThirdAxis (Data.GameSales -> Float, String)
    | ChangeFourthAxis (Data.GameSales -> Float, String)
    | ChangeFifthAxis (Data.GameSales -> Float, String)

    {--
    | TauschA
    | TauschB
    | TauschC
--}
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
{--
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
--}
{--
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
    }--}
--from early version of scatterplot
--same concept as in Scatterplot


--we need floats for MultiDimPoint so we need to change gamesales to float
type Model
  = Error
  | Loading
  | Success 
    { data : List Data.GameSales
    , genre : String
    , axis1 : Data.GameSales -> Float
    , axis2 : Data.GameSales -> Float
    , axis3 : Data.GameSales -> Float
    , axis4 : Data.GameSales -> Float
    , axis5 : Data.GameSales -> Float
    , name1 : String
    , name2 : String
    , name3 : String
    , name4 : String
    , name5 : String
    }
--}
{--
--updated with swap as Swap CustomType
--, swap : Swap
--}
{--
--exercise 6.1
type alias MultiDimPoint =
    { pointName : String
    , pointPublisher : String
    , value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }
    --}
{--
type AxisType
    = NorthAmerica
    | Europe
    | Japan
    | RestOfWorld
    | Global
--}
{--
--from scatterplot
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
{--
--filter and button from Scatterplot
filterGenre : List Data.GameSales -> String -> List Data.GameSales
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
--AxisChange to be integrated
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = Data.gamesSalesList [fullText], genre = "Action", axis1 = .northAmerica, axis2 = .europe, axis3 = .japan, axis4 = .restOfWorld, axis5 = .global, name1 = "North America", name2 = "Europe", name3 = "Japan", name4 = "Rest of World", name5 = "Global" }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        
        ChangeGenreType new_genre -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = new_genre, axis1 = a.axis1, axis2 = a.axis2, axis3 = a.axis3, axis4 = a.axis4, axis5 = a.axis5, name1 = a.name1, name2 = a.name2, name3 = a.name3, name4 = a.name4, name5 = a.name5 }, Cmd.none ) 
                _ ->
                    ( model, Cmd.none )

        ChangeFirstAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, axis1 = new_axis, axis2 = a.axis2, axis3 = a.axis3, axis4 = a.axis4, axis5 = a.axis5, name1 = new_name, name2 = a.name2, name3 = a.name3, name4 = a.name4, name5 = a.name5 }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeSecondAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, axis1 = a.axis1, axis2 = new_axis, axis3 = a.axis3, axis4 = a.axis4, axis5 = a.axis5, name1 = a.name1, name2 = new_name, name3 = a.name3, name4 = a.name4, name5 = a.name5 }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeThirdAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, axis1 = a.axis1, axis2 = a.axis2, axis3 = new_axis, axis4 = a.axis4, axis5 = a.axis5, name1 = a.name1, name2 = a.name2, name3 = new_name, name4 = a.name4, name5 = a.name5 }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeFourthAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, axis1 = a.axis1, axis2 = a.axis2, axis3 = a.axis3, axis4 = new_axis, axis5 = a.axis5, name1 = a.name1, name2 = a.name2, name3 = a.name3, name4 = new_name, name5 = a.name5 }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeFifthAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, axis1 = a.axis1, axis2 = a.axis2, axis3 = a.axis3, axis4 = a.axis4, axis5 = new_axis, name1 = a.name1, name2 = a.name2, name3 = a.name3, name4 = a.name4, name5 = new_name }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
                    --}
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
assignmentAndReduce : List GameSales -> List GameSales
assignmentAndReduce game =
    let
       assignment : GameSales -> Maybe GameSales
       assignment assign = 
            helpMapBig
                (GameSales assign.game assign.publisher assign.genre)
                (Just assign.northAmerica)
                (Just assign.europe)
                (Just assign.japan)
                (Just assign.restOfWorld)
                (Just assign.global) 
    in
    List.filterMap assignment game

--works, but not so pretty -> maybe find better solution
{--
--buttons for axis 1
button1axis1 : Html Msg
button1axis1 = Html.button [onClick (ChangeFirstAxis (.northAmerica, "North America"))][Html.text "North America"]

button2axis1 : Html Msg
button2axis1 = Html.button [onClick (ChangeFirstAxis (.europe, "Europe"))][Html.text "Europe"]
                        
button3axis1 : Html Msg                      
button3axis1 = Html.button [onClick (ChangeFirstAxis (.japan, "Japan"))][Html.text "Japan"]
  
button4axis1 : Html Msg
button4axis1 = Html.button [onClick (ChangeFirstAxis (.restOfWorld, "Rest of World"))][Html.text "Rest of World"]
 
button5axis1 : Html Msg
button5axis1 = Html.button [onClick (ChangeFirstAxis (.global, "Global"))][Html.text "Global"]             

-- buttons for axis 2
button1axis2 : Html Msg
button1axis2 = Html.button [onClick (ChangeSecondAxis (.northAmerica, "North America"))][Html.text "North America"]

button2axis2 : Html Msg
button2axis2 = Html.button [onClick (ChangeSecondAxis (.europe, "Europe"))][Html.text "Europe"]
                        
button3axis2 : Html Msg                      
button3axis2 = Html.button [onClick (ChangeSecondAxis (.japan, "Japan"))][Html.text "Japan"]
  
button4axis2 : Html Msg
button4axis2 = Html.button [onClick (ChangeSecondAxis (.restOfWorld, "Rest of World"))][Html.text "Rest of World"]
 
button5axis2 : Html Msg
button5axis2 = Html.button [onClick (ChangeSecondAxis (.global, "Global"))][Html.text "Global"] 

--buttons for axis 3
button1axis3 : Html Msg
button1axis3 = Html.button [onClick (ChangeThirdAxis (.northAmerica, "North America"))][Html.text "North America"]

button2axis3 : Html Msg
button2axis3 = Html.button [onClick (ChangeThirdAxis (.europe, "Europe"))][Html.text "Europe"]
                        
button3axis3 : Html Msg                      
button3axis3 = Html.button [onClick (ChangeThirdAxis (.japan, "Japan"))][Html.text "Japan"]
  
button4axis3 : Html Msg
button4axis3 = Html.button [onClick (ChangeThirdAxis (.restOfWorld, "Rest of World"))][Html.text "Rest of World"]
 
button5axis3 : Html Msg
button5axis3 = Html.button [onClick (ChangeThirdAxis (.global, "Global"))][Html.text "Global"] 

--buttons for axis 4
button1axis4 : Html Msg
button1axis4 = Html.button [onClick (ChangeFourthAxis (.northAmerica, "North America"))][Html.text "North America"]

button2axis4 : Html Msg
button2axis4 = Html.button [onClick (ChangeFourthAxis (.europe, "Europe"))][Html.text "Europe"]
                        
button3axis4 : Html Msg                      
button3axis4 = Html.button [onClick (ChangeFourthAxis (.japan, "Japan"))][Html.text "Japan"]
  
button4axis4 : Html Msg
button4axis4 = Html.button [onClick (ChangeFourthAxis (.restOfWorld, "Rest of World"))][Html.text "Rest of World"]
 
button5axis4 : Html Msg
button5axis4 = Html.button [onClick (ChangeFourthAxis (.global, "Global"))][Html.text "Global"] 


--buttons for axis 5
button1axis5 : Html Msg
button1axis5 = Html.button [onClick (ChangeFifthAxis (.northAmerica, "North America"))][Html.text "North America"]

button2axis5 : Html Msg
button2axis5 = Html.button [onClick (ChangeFifthAxis (.europe, "Europe"))][Html.text "Europe"]
                        
button3axis5 : Html Msg                      
button3axis5 = Html.button [onClick (ChangeFifthAxis (.japan, "Japan"))][Html.text "Japan"]
  
button4axis5 : Html Msg
button4axis5 = Html.button [onClick (ChangeFifthAxis (.restOfWorld, "Rest of World"))][Html.text "Rest of World"]
 
button5axis5 : Html Msg
button5axis5 = Html.button [onClick (ChangeFifthAxis (.global, "Global"))][Html.text "Global"] 
--}
{--
--from Scatterplot buttons & extended by number of axis
--does not work now with dropdown menu -> going for easier version with simple buttons to click on

buttonAxis1 : Html Msg
buttonAxis1 =
    Html.select
        [ onInput (ChangeFirstAxis (.northAmerica, "NorthAmerica"))]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonAxis2 : Html Msg
buttonAxis2 =
    Html.select
        [ onInput (\rx -> stringToAxisType rx |> ChangeSecondAxis) ]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonAxis3 : Html Msg
buttonAxis3 =
    Html.select
        [ onInput (\rx -> stringToAxisType rx |> ChangeThirdAxis) ]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonAxis4 : Html Msg
buttonAxis4 =
    Html.select
        [ onInput (\rx -> stringToAxisType rx |> ChangeFourthAxis) ]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonAxis5 : Html Msg
buttonAxis5 =
    Html.select
        [ onInput (\rx -> stringToAxisType rx |> ChangeFithAxis) ]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]
--}
{--
--from exercise 6.3
type alias Swap =
    { wertCityMPG : Int
    , attributWert : Int
    , accessWerte : List ( String, MultiPoint -> Int )
    , accessWerte2 : List ( String, MultiPoint -> Int )
    , wert1 : String
    , wert2 : String
    , wert3 : String
    , wert4 : String
    , wert5 : String
    }

my_access_function : Swap -> List ( String, MultiPoint -> Int )
my_access_function model =
    List.Extra.swapAt model.attributWert model.wertCityMPG model.accessWerte2
--}
{--
view : Model -> Html Msg
view model =
    case model of
        Error ->
            text "Opening the data for sales of games on XBoxOne failed"

        Loading ->
            text "Loading GameSales data..."
        
        Success fullText ->
            let
                
            in
            Html.div [Html.Attributes.style "padding" "10px"]
                [ Html.h1 [Html.Attributes.style "fontSize" "30px"] 
                    [ Html.text ("Parallel Coordinates Plot of Video Game Sales for XBox One") ]
                , Html.h2 [Html.Attributes.style "fontSize" "20px"] 
                --to be specified and explained more
                    [ Html.text ("This parallel coordinates plot shows the sales of video games in millions of units for XBox One sorted by selected genre.") ]
                , Html.p [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Number of all games across all genres: " ++ String.fromInt number_games)]
                , Html.p [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Number of all games across all genres: " ++ String.fromInt number_games_cleared)]
                , Html.h4 [Html.Attributes.style "fontSize" "16px"]
                    [ Html.text ("Please choose the genre you want to display with the button below.") ]
                , Html.p [Html.Attributes.style "padding" "10px"]
                    [ buttonGenreType ]
                , Html.p [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Number of games in selected genre: " ++ String.fromInt number_games_genre)]
                , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Please choose the region you want to display on the first axis with the adjacent buttons: ")
                        , button1axis1
                        , button2axis1
                        , button3axis1
                        , button4axis1
                        , button5axis1]
                , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Please choose the region you want to display on the second axis with the adjacent buttons: ") 
                        , button1axis2
                        , button2axis2
                        , button3axis2
                        , button4axis2
                        , button5axis2]
                , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Please choose the region you want to display on the third axis with the adjacent buttons: ")
                        , button1axis3
                        , button2axis3
                        , button3axis3
                        , button4axis3
                        , button5axis3 ]
                , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Please choose the region you want to display on the fourth axis with the adjacent buttons: ")
                        , button1axis4
                        , button2axis4
                        , button3axis4
                        , button4axis4
                        , button5axis4 ]
                , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                    [ Html.text ("Please choose the region you want to display on the fifth axis with the adjacent buttons: ")
                        , button1axis5
                        , button2axis5
                        , button3axis5
                        , button4axis5
                        , button5axis5 ]
                , Html.h2 [Html.Attributes.style "fontSize" "20px"]
                    [Html.text ("Parallel Coordinates Plot for " ++ fullText.genre )]
                , scatterplotParallel cssParallel 600 2 multiDimFunction
                ]
--}
{--
gameSalesData: List Data.GameSales
gameSalesData = 
    fullText.data
                  
number_games: Int
number_games =
    List.length gameSalesData
                
clearedGameSalesData : List Data.GameSales
clearedGameSalesData = 
    assignmentAndReduce gameSalesData

number_games_cleared : Int
number_games_cleared = 
    List.length clearedGameSalesData

number_games_genre: Int
number_games_genre =  
    List.length gameSalesDataFiltered
                --}
                --changed again to originally desired concept of selecting not swapping       
                --adjusted -> no more AxisType, insteas GameSales -> Floats
                --same reason for parenthesis around (a1 data) etc
                --still not applied
-- now can use RegionType when having a conversion from RegionType to needed (GameSales -> Float) to get Floats for MultiDimData
multiDimenData : List GameSales -> RegionType -> RegionType -> RegionType -> RegionType -> RegionType -> (GameSales -> String) -> (GameSales -> String) -> String -> String -> String -> String -> String -> MultiDimData
multiDimenData game a1 a2 a3 a4 a5 name pub na1 na2 na3 na4 na5=
    MultiDimData [na1, na2, na3, na4, na5]
    --[Data.regionTypeToString Data.NorthAmerica, Data.regionTypeToString Data.Europe, Data.regionTypeToString Data.Japan, Data.regionTypeToString Data.RestOfWorld, Data.regionTypeToString Data.Global]
        [ List.map
            (\data ->
                [  (regionTypeToAxisAnnotation a1 data) , (regionTypeToAxisAnnotation a2 data), (regionTypeToAxisAnnotation a3 data), (regionTypeToAxisAnnotation a4 data), (regionTypeToAxisAnnotation a5 data) ]
                    |> MultiDimPoint (name data) (pub data)
            )
            game
        ]
{--
                --to apply multiDimenData with genrefilter & real data
multiDimFunction = 
multiDimenData gameSalesDataFiltered fullText.axis1 fullText.axis2 fullText.axis3 fullText.axis4 fullText.axis5 .game .publisher fullText.name1 fullText.name2 fullText.name3 fullText.name4 fullText.name5
            
                --from Scatterplot to fit multiDimenData (filteredGamesGenre doesn't)
gameSalesDataFiltered = 
    filterGenre fullText.data fullText.genre
--}


{--
                --from exercise 6.3
                --didn't want swap, wanted explicit selection like in Scatterplot
                --but just to try if this works better/at all
                --problems with the Tuple.second -> might be problems with Model and solution of a CustomType Swap instead of swap beeing the model in original exercise 6.3
                --sometimes throws error, sometimes not 
                --still problem if it fo whatever reason doesn't throw an error: can't directly acces record fields of swap in update for changes & initation
                --might change to swap again later in process of composing visualisations & interactions
                multiDimensionaleDaten =
                    MultiDimData (List.map Tuple.first (my_access_function fullText.swap))
                        [ List.map
                            (\data ->
                                List.map (\access -> Tuple.second access data) (my_access_function fullText.swap)
                                    |> List.map toFloat
                                    |> MultiDimPoint data.pointGame data.pointPublisher
                            )
                            filteredGamesGenre
                        ]
--}

{--
--from exercise 6.1
                filteredGamesGenre : List MultiPoint
                filteredGamesGenre =
                    assignmentAndReduce fullText.data
                        |> List.filter
                        (.pointGenre >> (==) fullText.genre)
--}

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

--opacity not 1 in normal to get the same effect as with x-ray but still white rectangle
cssParallel : String
cssParallel = 
    """
        .cssparallel { stroke: rgba(46, 78, 23, 0.8); opacity: 0.5 }
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