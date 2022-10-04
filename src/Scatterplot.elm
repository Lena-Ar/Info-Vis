module Scatterplot exposing (..)

import Browser
import Csv exposing (parse)
import Csv.Decode exposing (..)
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