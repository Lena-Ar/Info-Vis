module Data exposing (..)

import Csv exposing (parse)
import Csv.Decode exposing (..)
import Tree exposing (Tree)
import Http

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


--for all plots
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

type RegionType
    = NorthAmerica
    | Europe
    | Japan
    | RestOfWorld
    | Global

type PlotType
    = ParallelPlot
    | Scatterplot
    | TreeHierarchy

--for Scatterplot
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

--for ParallelPlot
type alias MultiDimPoint =
    { pointName : String
    , pointPublisher : String
    , value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }

--for TreeHierarchy
--list of floats for values of child and parent x and y and label as string
type alias NodeValues =
    { childx : Float
    , childy : Float
    , parentx : Float
    , parenty : Float
    , label : String
    }

type alias Model =
    { tree : Tree String, errorMsg : String }

type Msg
    = GotFlare (Result Http.Error (Tree String))
    
stringToPlotType : String -> PlotType
stringToPlotType plotType = 
    case plotType of
        "Scatterplot" -> 
            Scatterplot
        
        "Tree Hierarchy" -> 
            TreeHierarchy

        _ -> 
            ParallelPlot

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

--Umwandlung zu der Typannotation, die auch im Msg definiert ist, damit dann der dropdown-button klappen kann
--Anpassung als output RegionType, String wie im angepassten Msg
stringToAxisType : String -> (RegionType, String)
stringToAxisType stringAxisType =
    if stringAxisType == "North America" then
        (NorthAmerica, "North America")

    else if stringAxisType == "Europe" then
        (Europe,"Europe" )

    else if stringAxisType == "Japan" then
        (Japan, "Japan")

    else if stringAxisType == "Rest of world" then
        (RestOfWorld, "Rest of World")

    else
        (Global, "Global")

regionTypeToAxisAnnotation : RegionType -> (GameSales -> Float)
regionTypeToAxisAnnotation regionType =
    case regionType of
        NorthAmerica -> 
            (.northAmerica)

        Europe ->
            (.europe)

        Japan ->
            (.japan)

        RestOfWorld ->
            (.restOfWorld)

        Global ->
            (.global)