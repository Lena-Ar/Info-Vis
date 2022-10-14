module Data exposing (..)

import Csv exposing (parse)
import Csv.Decode exposing (..)

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