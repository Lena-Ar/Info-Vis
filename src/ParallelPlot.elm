module ParallelPlot exposing (..)

import Browser
import Csv
import Csv.Decode
import Html exposing (Html, pre, text)
import Http
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

daten : List String
daten =
    [ "XBoxOne_GameSales_test" ]

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , loadingGameSales GotText
    )

loadingGameSales : (Result Http.Error String -> Msg) -> Cmd Msg
loadingGameSales game = 
    daten
        |> List.map
            (\d ->
                Http.get
                    { url = "../Daten/CSV" ++ d
                    , expect = Http.expectString game
                    }
            )
        |> Cmd.batch

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
  | Success 
    { data : List GameSales
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
    List.map(\t -> csvString_to_data t) listGame
        |> List.concat

--cases for buttons to be added
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = gamesSalesList [ fullText ]}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



--view to be coded
{--
view : Model -> Html Msg
view model =
    case model of
        Error ->
            Html.text "Opening the data for sales of games on XBoxOne failed"

        Loading ->
            Html.text "Loading GameSales data..."
        
        Success 
--}