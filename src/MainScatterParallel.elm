module MainScatterParallel exposing (..)

import Browser
import Http
import Csv exposing (parse)
import Csv.Decode exposing (..)
import Data
import Html exposing (Html, pre, text)
import Html.Attributes exposing (href, placeholder, type_, value, target)
import Html.Events exposing (..)
import ParallelPlot
import Scatterplot


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
    , axis1 : Data.RegionType
    , axis2 : Data.RegionType
    , axis3 : Data.RegionType
    , axis4 : Data.RegionType
    , axis5 : Data.RegionType
    , name1 : String
    , name2 : String
    , name3 : String
    , name4 : String
    , name5 : String
    , xaxis: Data.RegionType
    , yaxis: Data.RegionType
    , plot : Data.PlotType
    }

type Msg
    = GotText (Result Http.Error String)
    | ChangeGenreType String
    | ChangeFirstAxis (Data.RegionType, String)
    | ChangeSecondAxis (Data.RegionType, String)
    | ChangeThirdAxis (Data.RegionType, String)
    | ChangeFourthAxis (Data.RegionType, String)
    | ChangeFifthAxis (Data.RegionType, String)
    | ChangeRegionX Data.RegionType
    | ChangeRegionY Data.RegionType
    | ChangePlot Data.PlotType

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = Data.gamesSalesList [fullText], genre = "Action", plot = Data.ParallelPlot, axis1 = Data.NorthAmerica, axis2 = Data.Europe, axis3 = Data.Japan, axis4 = Data.RestOfWorld, axis5 = Data.Global, name1 = Data.regionTypeToString Data.NorthAmerica, name2 = Data.regionTypeToString Data.Europe, name3 = Data.regionTypeToString Data.Japan, name4 = Data.regionTypeToString Data.RestOfWorld, name5 = Data.regionTypeToString Data.Global, xaxis = Data.NorthAmerica, yaxis = Data.NorthAmerica }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        
        ChangeGenreType new_genre -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = new_genre, plot = a.plot, axis1 = a.axis1, axis2 = a.axis2, axis3 = a.axis3, axis4 = a.axis4, axis5 = a.axis5, name1 = a.name1, name2 = a.name2, name3 = a.name3, name4 = a.name4, name5 = a.name5, xaxis = a.xaxis, yaxis = a.yaxis }, Cmd.none ) 
                _ ->
                    ( model, Cmd.none )
        
        ChangePlot new_plot ->
            case model of
                Success a -> 
                    (Success <| { data = a.data, genre = a.genre, plot = new_plot, axis1 = a.axis1, axis2 = a.axis2, axis3 = a.axis3, axis4 = a.axis4, axis5 = a.axis5, name1 = a.name1, name2 = a.name2, name3 = a.name3, name4 = a.name4, name5 = a.name5, xaxis = a.xaxis, yaxis = a.yaxis}, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )

        ChangeFirstAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, plot = a.plot, axis1 = new_axis, axis2 = a.axis2, axis3 = a.axis3, axis4 = a.axis4, axis5 = a.axis5, name1 = new_name, name2 = a.name2, name3 = a.name3, name4 = a.name4, name5 = a.name5, xaxis = a.xaxis, yaxis = a.yaxis }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeSecondAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, plot = a.plot, axis1 = a.axis1, axis2 = new_axis, axis3 = a.axis3, axis4 = a.axis4, axis5 = a.axis5, name1 = a.name1, name2 = new_name, name3 = a.name3, name4 = a.name4, name5 = a.name5, xaxis = a.xaxis, yaxis = a.yaxis }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeThirdAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, plot = a.plot, axis1 = a.axis1, axis2 = a.axis2, axis3 = new_axis, axis4 = a.axis4, axis5 = a.axis5, name1 = a.name1, name2 = a.name2, name3 = new_name, name4 = a.name4, name5 = a.name5, xaxis = a.xaxis, yaxis = a.yaxis }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeFourthAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, plot = a.plot, axis1 = a.axis1, axis2 = a.axis2, axis3 = a.axis3, axis4 = new_axis, axis5 = a.axis5, name1 = a.name1, name2 = a.name2, name3 = a.name3, name4 = new_name, name5 = a.name5, xaxis = a.xaxis, yaxis = a.yaxis }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeFifthAxis (new_axis, new_name) -> 
            case model of
                Success a ->
                    (Success <| { data = a.data, genre = a.genre, plot = a.plot, axis1 = a.axis1, axis2 = a.axis2, axis3 = a.axis3, axis4 = a.axis4, axis5 = new_axis, name1 = a.name1, name2 = a.name2, name3 = a.name3, name4 = a.name4, name5 = new_name, xaxis = a.xaxis, yaxis = a.yaxis  }, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeRegionX new_regionx -> 
            case model of
                Success a -> 
                    (Success <| { data = a.data, genre = a.genre, plot = a.plot, axis1 = a.axis1, axis2 = a.axis2, axis3 = a.axis3, axis4 = a.axis4, axis5 = a.axis5, name1 = a.name1, name2 = a.name2, name3 = a.name3, name4 = a.name4, name5 = a.name5, xaxis = new_regionx, yaxis = a.yaxis}, Cmd.none ) 
                _ -> 
                    ( model, Cmd.none )
        
        ChangeRegionY new_regiony ->
            case model of
                Success a -> 
                    (Success <| { data = a.data, genre = a.genre, plot = a.plot, axis1 = a.axis1, axis2 = a.axis2, axis3 = a.axis3, axis4 = a.axis4, axis5 = a.axis5, name1 = a.name1, name2 = a.name2, name3 = a.name3, name4 = a.name4, name5 = a.name5, xaxis = a.xaxis, yaxis = new_regiony}, Cmd.none ) 
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
                
                --from Scatterplot to fit multiDimenData (filteredGamesGenre doesn't)
                gameSalesDataFiltered = 
                    filterGenre fullText.data fullText.genre
                number_games_genre: Int
                number_games_genre =  
                    List.length gameSalesDataFiltered

            in
            case fullText.plot of 
                Data.ParallelPlot -> 
                    let
                        --parallelPlot
                        gameSalesDataNull : List Data.GameSales
                        gameSalesDataNull = 
                            ParallelPlot.assignmentAndReduce gameSalesData
                
                        number_games_null : Int
                        number_games_null = 
                            List.length gameSalesDataNull

                        clearedGameSalesData : List Data.GameSales
                        clearedGameSalesData = 
                            ParallelPlot.assignmentAndReduce gameSalesDataFiltered
                
                        number_games_cleared : Int
                        number_games_cleared = 
                            List.length clearedGameSalesData

                        multiDimFunction = 
                            ParallelPlot.multiDimenData clearedGameSalesData fullText.axis1 fullText.axis2 fullText.axis3 fullText.axis4 fullText.axis5 .game .publisher fullText.name1 fullText.name2 fullText.name3 fullText.name4 fullText.name5
                    in
                    Html.div [Html.Attributes.style "padding" "10px", Html.Attributes.style "background" "rgba(0, 0, 0, 0.009)"]
                        [ Html.div [Html.Attributes.style "text-align" "center", Html.Attributes.style "margin" "auto", Html.Attributes.style "color" "rgba(46, 78, 23,1)"]
                            [ Html.h1 [Html.Attributes.style "fontSize" "40px"]
                                [Html.text ("Detailed information on Video Games Sales for XBox One")]
                            ]
                        , Html.div [Html.Attributes.style "margin" "2% 1% 2% 1%"
                                    , Html.Attributes.style "padding" "5px 10px 5px 10px"
                                    , Html.Attributes.style "border-style" "double"
                                    , Html.Attributes.style "border-radius" "10px"
                                    , Html.Attributes.style "border-color" "rgba(46, 78, 23,0.7)"
                                    , Html.Attributes.style "background" "rgba(75, 128, 36,0.08)"
                                    , Html.Attributes.style "fontSize" "16px"]
                            [ Html.div []
                                [ Html.h4 []
                                    [Html.text ("Please choose which plot you would like to see.")]
                                , Html.p []
                                    [buttonPlot]
                                ,   Html.div [ Html.Attributes.style "padding" "5px 10px 5px 10px"
                                            , Html.Attributes.style "border-style" "dotted"
                                            , Html.Attributes.style "border-radius" "10px"
                                            , Html.Attributes.style "border-color" "rgba(46, 78, 23,0.2)"
                                            , Html.Attributes.style "border-width" "1px"
                                            , Html.Attributes.style "background" "rgba(75, 128, 36,0.03)"
                                            ]
                                    [ Html.h4 [Html.Attributes.style "fontSize" "16px"]
                                        [Html.text ("Please choose the genre you want to display with the button below.")]
                                    , Html.p [Html.Attributes.style "padding" "10px"]
                                        [buttonGenreType]
                                    , Html.text ("Don't worry, your settings for genre will stay selected.")
                                ]
                                ]
                            ]
                        , Html.div [Html.Attributes.style "margin" "auto"
                                    , Html.Attributes.style "background" "rgba(0, 0, 0, 0.02)"
                                    , Html.Attributes.style "border-style" "solid"
                                    , Html.Attributes.style "border-width" "0.2px"
                                    , Html.Attributes.style "border-radius" "10px"
                                    , Html.Attributes.style "border-color" "rgba(0, 0, 0, 0.05)"
                                    ] 
                            [ Html.div [Html.Attributes.style "padding" "5px 10px 5px 10px"] 
                               [ Html.h1 [Html.Attributes.style "fontSize" "30px", Html.Attributes.style "color" "rgba(75, 128, 36, 1)", Html.Attributes.style "text-align" "center"] 
                                    [ Html.text ("Parallel Coordinates Plot of Video Game Sales for XBox One") ]
                                , Html.p [Html.Attributes.style "fontSize" "18px", Html.Attributes.style "text-align" "center"] 
                        --to be specified and explained more
                                    [ Html.text ("This parallel coordinates plot shows the sales of video games in millions of units for XBox One sorted by selected genre.") ]
                               ]
                            , Html.div [Html.Attributes.style "padding" "5px 10px 5px 10px"]
                                [ Html.p [Html.Attributes.style "fontSize" "15px"]
                                    [ Html.text ("Number of all games across all genres: " ++ String.fromInt number_games)
                                    , Html.br [][]
                                    , Html.text ("Number of all games across all genres cleared by potential Null-values: " ++ String.fromInt number_games_null)
                                    ]
                                , Html.p [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Number of games in selected genre " ++ fullText.genre ++ ": " ++ String.fromInt number_games_cleared)]
                                , Html.div [Html.Attributes.style "padding" "5px 10px 5px 10px"
                                            , Html.Attributes.style "border-style" "dotted"
                                            , Html.Attributes.style "border-radius" "10px"
                                            , Html.Attributes.style "border-color" "rgba(46, 78, 23,0.1)"
                                            , Html.Attributes.style "border-width" "1px"
                                            , Html.Attributes.style "background" "rgba(75, 128, 36,0.02)"]
                                    [ Html.p [Html.Attributes.style "fontSize" "13px"]
                                        [Html.text ("Hint: All axes can be adjusted to your needs by seleceting the needed regions for each axis with the buttons below.")]
                                    , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Please choose the region you want to display on the first axis with the adjacent buttons: ")
                                            , buttonAxis1
                                {--
                                , button1axis1
                                , button2axis1
                                , button3axis1
                                , button4axis1
                                , button5axis1--}
                                            ]
                                    , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Please choose the region you want to display on the second axis with the adjacent buttons: ") 
                                            , buttonAxis2
                                {--
                                , button1axis2
                                , button2axis2
                                , button3axis2
                                , button4axis2
                                , button5axis2--}
                                            ]
                                    , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Please choose the region you want to display on the third axis with the adjacent buttons: ")
                                            , buttonAxis3
                                {--
                                , button1axis3
                                , button2axis3
                                , button3axis3
                                , button4axis3
                                , button5axis3 --}
                                            ]
                                    , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Please choose the region you want to display on the fourth axis with the adjacent buttons: ")
                                        , buttonAxis4
                                {--
                                , button1axis4
                                , button2axis4
                                , button3axis4
                                , button4axis4
                                , button5axis4 --}
                                        ]
                                    , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Please choose the region you want to display on the fifth axis with the adjacent buttons: ")
                                            , buttonAxis5
                                {--
                                , button1axis5
                                , button2axis5
                                , button3axis5
                                , button4axis5
                                , button5axis5 --}
                                        ]
                                    ]
                                ]
                            , Html.h2 [Html.Attributes.style "fontSize" "20px", Html.Attributes.style "padding" "5px 5px 10px 10px", Html.Attributes.style "text-align" "center", Html.Attributes.style "color" "rgb(75, 128, 36, 0.75)"]
                                [Html.text ("Parallel Coordinates Plot for " ++ fullText.genre++ " with " ++ (Data.regionTypeToString fullText.axis1) ++ " as first axis, " ++ (Data.regionTypeToString fullText.axis2) ++ " as second axis, " ++ (Data.regionTypeToString fullText.axis3) ++ " as third axis, " ++ (Data.regionTypeToString fullText.axis4) ++ " as fourth axis and " ++ (Data.regionTypeToString fullText.axis5) ++ " as fifth axis.")]
                            , Html.div [Html.Attributes.style "padding" "5px"]
                                [ParallelPlot.scatterplotParallel ParallelPlot.cssParallel 600 2 multiDimFunction]
                            ]
                        , Html.p [Html.Attributes.style "fontSize" "15px", Html.Attributes.style "color" "rgba(0, 0, 0, 0.57)"] 
                            [ Html.text ("You would like to go back to the Tree Diagramm/Tree Hierarchy? No problem, this link will ")
                            , Html.a [href "TreeHierarchy.elm", target "_blank", Html.Attributes.style "color" "rgb(75, 128, 36, 0.75)"] 
                                [Html.text "take you back and open another tab!"]
                            ]
                        ]
                Data.Scatterplot ->
                    let
                        --scatterplot
                        gameSalesDataNull =
                            Scatterplot.filterAndReduceGames (fullText.data)
            
                        number_clearedGames: Int
                        number_clearedGames = 
                            List.length gameSalesDataNull.data
                
                        gameSalesDataCleared = 
                            Scatterplot.filterAndReduceGames (gameSalesDataFiltered)
                
                        valuesX : List Float
                        valuesX = 
                            Scatterplot.regionFilter gameSalesDataFiltered fullText.xaxis


                        valuesY : List Float
                        valuesY = 
                            Scatterplot.regionFilter gameSalesDataFiltered fullText.yaxis
                    in
                    Html.div [Html.Attributes.style "padding" "10px", Html.Attributes.style "background" "rgba(0, 0, 0, 0.009)"]
                        [ Html.div [Html.Attributes.style "text-align" "center", Html.Attributes.style "margin" "auto", Html.Attributes.style "color" "rgba(46, 78, 23,1)"]
                            [Html.h1 [Html.Attributes.style "fontSize" "40px"]
                                [Html.text ("Detailed information on Video Games Sales for XBox One")]
                            ]
                        , Html.div [Html.Attributes.style "margin" "2% 1% 2% 1%"
                                    , Html.Attributes.style "padding" "5px 10px 5px 10px"
                                    , Html.Attributes.style "border-style" "double"
                                    , Html.Attributes.style "border-radius" "10px"
                                    , Html.Attributes.style "border-color" "rgba(46, 78, 23,0.7)"
                                    , Html.Attributes.style "background" "rgba(75, 128, 36,0.08)"
                                    , Html.Attributes.style "fontSize" "16px"]
                            [Html.div []
                                [ Html.h4 []
                                    [Html.text ("Please choose which plot you would like to see.")]
                                , Html.p []
                                    [buttonPlot]
                                , Html.div [ Html.Attributes.style "padding" "5px 10px 5px 10px"
                                            , Html.Attributes.style "border-style" "dotted"
                                            , Html.Attributes.style "border-radius" "10px"
                                            , Html.Attributes.style "border-color" "rgba(46, 78, 23,0.2)"
                                            , Html.Attributes.style "border-width" "1px"
                                            , Html.Attributes.style "background" "rgba(75, 128, 36,0.03)"
                                            ]
                                    [ Html.h4 [Html.Attributes.style "fontSize" "16px"]
                                        [Html.text ("Please choose the genre you want to display with the button below.")]
                                    , Html.p [Html.Attributes.style "padding" "10px"]
                                        [buttonGenreType]
                                    , Html.text ("Don't worry, your settings for genre will stay selected.")
                                    ]
                                ]
                            ]
                --scatterplot
                        , Html.div [Html.Attributes.style "margin" "auto"
                                    , Html.Attributes.style "background" "rgba(0, 0, 0, 0.02)"
                                    , Html.Attributes.style "border-style" "solid"
                                    , Html.Attributes.style "border-width" "0.2px"
                                    , Html.Attributes.style "border-radius" "10px"
                                    , Html.Attributes.style "border-color" "rgba(0, 0, 0, 0.05)"
                                    ]
                            [ Html.div [Html.Attributes.style "padding" "5px 10px 5px 10px"]
                                [ Html.h1 [Html.Attributes.style "fontSize" "30px", Html.Attributes.style "color" "rgba(75, 128, 36, 1)", Html.Attributes.style "text-align" "center"] 
                                    [ Html.text ("Scatterplot of Video Game Sales for XBox One") ]
                                , Html.p [Html.Attributes.style "fontSize" "18px", Html.Attributes.style "text-align" "center"] 
                                    [ Html.text ("This scatterplot shows the sales of video games in millions of units for XBox One sorted by selected genre.")]
                                ]
                            , Html.div [Html.Attributes.style "padding" "5px 10px 5px 10px"]
                                [ Html.p [Html.Attributes.style "fontSize" "15px"]
                                    [ Html.text ("Number of all games across all genres: " ++ String.fromInt number_games)
                                    , Html.br [] []
                                    , Html.text ("Number of all games across all genres cleared by potential Null-values: " ++ String.fromInt number_clearedGames)
                                    ]
                                , Html.p [Html.Attributes.style "fontSize" "15px"]
                                    [ Html.text ("Number of games in selected genre " ++ fullText.genre ++ ": " ++ String.fromInt number_games_genre) ]
                                , Html.div [Html.Attributes.style "padding" "5px 10px 5px 10px"
                                            , Html.Attributes.style "border-style" "dotted"
                                            , Html.Attributes.style "border-radius" "10px"
                                            , Html.Attributes.style "border-color" "rgba(46, 78, 23,0.1)"
                                            , Html.Attributes.style "border-width" "1px"
                                            , Html.Attributes.style "background" "rgba(75, 128, 36,0.02)"]
                                    [ Html.p [Html.Attributes.style "fontSize" "13px"]
                                        [Html.text ("Hint: The x-axis and y-axis can be adjusted to your needs by seleceting the needed regions for each axis with the buttons below.")]
                                    , Html.h4 [Html.Attributes.style "fontSize" "16px"]
                                        [ Html.text ("Please choose the region you want to display on the xaxis first (on the left) and the one you want to display on the yaxis second (on the right).")]
                                    , Html.p [Html.Attributes.style "padding" "10px"]
                                        [ buttonRegionTypeX
                                        , buttonRegionTypeY ]
                                    ]
                                ]
                            , Html.h2 [Html.Attributes.style "fontSize" "20px", Html.Attributes.style "padding" "5px 5px 10px 10px", Html.Attributes.style "text-align" "center", Html.Attributes.style "color" "rgb(75, 128, 36, 0.75)"]
                                [Html.text ("Scatterplot for " ++ fullText.genre ++ " with " ++ (Data.regionTypeToString fullText.xaxis) ++ " as x-axis and " ++ (Data.regionTypeToString fullText.yaxis) ++ " as y-axis.")]
                            , Html.div [Html.Attributes.style "padding" "5px"]
                                [Scatterplot.scatterplot Scatterplot.cssPoint gameSalesDataCleared valuesX valuesY (Data.regionTypeToString fullText.xaxis) (Data.regionTypeToString fullText.yaxis)]
                                    
                            ]
                        , Html.p [Html.Attributes.style "fontSize" "15px", Html.Attributes.style "color" "rgba(0, 0, 0, 0.57)"] 
                            [ Html.text ("You would like to go back to the Tree Diagramm/Tree Hierarchy? No problem, this link will ")
                            , Html.a [href "TreeHierarchy.elm", target "_blank", Html.Attributes.style "color" "rgb(75, 128, 36, 0.75)"] 
                                [Html.text "take you back and open another tab!"]
                            ]
                        ]
                Data.TreeHierarchy -> 
                    Html.div [Html.Attributes.style "padding" "10px", Html.Attributes.style "background" "rgba(0, 0, 0, 0.009)"]
                        [ Html.div [Html.Attributes.style "text-align" "center", Html.Attributes.style "margin" "auto", Html.Attributes.style "color" "rgba(46, 78, 23,1)"]
                            [ Html.h1 [Html.Attributes.style "fontSize" "40px"]
                                [Html.text ("General overview of Video Games, Genres and Publishers for XBox One")]
                            ]
                            , Html.div [Html.Attributes.style "margin" "2% 1% 2% 1%"
                                    , Html.Attributes.style "padding" "5px 10px 5px 10px"
                                    , Html.Attributes.style "border-style" "double"
                                    , Html.Attributes.style "border-radius" "25px"
                                    , Html.Attributes.style "border-color" "rgba(46, 78, 23,0.7)"
                                    , Html.Attributes.style "background" "rgba(75, 128, 36,0.08)"
                                    , Html.Attributes.style "fontSize" "16px"]
                                [ Html.h4 []
                                    [ Html.text ("Please choose which plot you would like to see.")
                                    , Html.p []
                                        [buttonPlot]
                                    , Html.text ("Don't worry, your settings for genre will stay selected.")]
                                ]
                                , Html.div [Html.Attributes.style "margin" "auto"
                                    , Html.Attributes.style "background" "rgba(0, 0, 0, 0.02)"
                                    , Html.Attributes.style "border-style" "solid"
                                    , Html.Attributes.style "border-width" "0.2px"
                                    , Html.Attributes.style "border-radius" "10px"
                                    , Html.Attributes.style "border-color" "rgba(0, 0, 0, 0.05)"
                                    ] 
                                    [ Html.div [Html.Attributes.style "padding" "5px 10px 5px 10px"]
                                        [ Html.h1 [Html.Attributes.style "fontSize" "30px"] 
                                            [ Html.text ("Tree Diagram/Tree Hierarchy of Video Games for XBox One") ]
                                        ]
                                    , Html.div [Html.Attributes.style "padding" "5px 10px 5px 10px"]
                                        [ Html.h4 [Html.Attributes.style "fontSize" "16px"] 
                                        [ Html.text ("If you want to display the Tree Diagram/Tree Hierarchy you saw in the beginning, you can ")
                                        , Html.a [href "TreeHierarchy.elm", Html.Attributes.style "color" "rgba(153, 17, 17, 1)"] 
                                            [Html.text "click here!"]
                                        ]
                                        ]
                                ]
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
    
buttonAxis1 : Html Msg
buttonAxis1 = 
    Html.select
        [ onInput (\rx -> Data.stringToAxisType rx |> ChangeFirstAxis)]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonAxis2 : Html Msg
buttonAxis2 = 
    Html.select
        [ onInput (\rx -> Data.stringToAxisType rx |> ChangeSecondAxis)]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonAxis3 : Html Msg
buttonAxis3 = 
    Html.select
        [ onInput (\rx -> Data.stringToAxisType rx |> ChangeThirdAxis)]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonAxis4 : Html Msg
buttonAxis4 = 
    Html.select
        [ onInput (\rx -> Data.stringToAxisType rx |> ChangeFourthAxis)]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonAxis5 : Html Msg
buttonAxis5 = 
    Html.select
        [ onInput (\rx -> Data.stringToAxisType rx |> ChangeFifthAxis)]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]
{--
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
buttonRegionTypeX : Html Msg
buttonRegionTypeX =
    Html.select
        [ onInput (\rx -> Data.stringToRegionType rx |> ChangeRegionX) ]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonRegionTypeY : Html Msg
buttonRegionTypeY =
    Html.select
        [ onInput (\ry -> Data.stringToRegionType ry |> ChangeRegionY) ]
        [ Html.option [ value "North America" ] [ Html.text "North America" ]
        , Html.option [ value "Europe" ] [ Html.text "Europe" ]
        , Html.option [ value "Japan" ] [ Html.text "Japan" ]
        , Html.option [ value "Rest of world" ] [ Html.text "Rest of world" ]
        , Html.option [ value "Global" ] [ Html.text "Global" ]
        ]

buttonPlot : Html Msg
buttonPlot =
    Html.select
        [ onInput (\ry -> Data.stringToPlotType ry |> ChangePlot) ]
        [ Html.option [ value "ParallelPlot" ] [ Html.text "Parallel Coordinate Plot" ]
        , Html.option [ value "Scatterplot" ] [ Html.text "Scatterplot" ]
        , Html.option [value "Tree Hierarchy"] [Html.text "Tree Hierarchy"]
        ]