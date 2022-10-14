module MainScatterParallel exposing (..)

import Browser
import Http
import Csv exposing (parse)
import Csv.Decode exposing (..)
import Data
import Html exposing (Html, pre, text)
import Html.Attributes exposing (href, placeholder, type_, value)
import Html.Events exposing (..)
import ParallelPlot


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "https://raw.githubusercontent.com/Lena-Ar/Info-Vis/main/Daten/CSV/XboxOne_GameSales_test.csv"
        , expect = Http.expectString GotText
        }
    )

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

type Msg
    = GotText (Result Http.Error String)
    | ChangeGenreType String
    | ChangeFirstAxis (Data.GameSales -> Float, String)
    | ChangeSecondAxis (Data.GameSales -> Float, String)
    | ChangeThirdAxis (Data.GameSales -> Float, String)
    | ChangeFourthAxis (Data.GameSales -> Float, String)
    | ChangeFifthAxis (Data.GameSales -> Float, String)

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

filterGenre : List Data.GameSales -> String -> List Data.GameSales
filterGenre allGames genretype =
    List.filter (\c -> c.genre == genretype) allGames


view : Model -> Html Msg
view model =
    case model of
        Error ->
            text "Opening the data for sales of games on XBoxOne failed"

        Loading ->
            text "Loading GameSales data..."
        
        Success fullText ->
            let
                gameSalesData: List Data.GameSales
                gameSalesData = 
                    fullText.data
                
                number_games: Int
                number_games =
                    List.length gameSalesData
                
                clearedGameSalesData : List Data.GameSales
                clearedGameSalesData = 
                    ParallelPlot.assignmentAndReduce gameSalesData
                
                number_games_cleared : Int
                number_games_cleared = 
                    List.length clearedGameSalesData

                number_games_genre: Int
                number_games_genre =  
                    List.length gameSalesDataFiltered

                multiDimFunction = 
                    ParallelPlot.multiDimenData gameSalesDataFiltered fullText.axis1 fullText.axis2 fullText.axis3 fullText.axis4 fullText.axis5 .game .publisher fullText.name1 fullText.name2 fullText.name3 fullText.name4 fullText.name5
            
                --from Scatterplot to fit multiDimenData (filteredGamesGenre doesn't)
                gameSalesDataFiltered = 
                    filterGenre fullText.data fullText.genre
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
                , ParallelPlot.scatterplotParallel ParallelPlot.cssParallel 600 2 multiDimFunction
                ]

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