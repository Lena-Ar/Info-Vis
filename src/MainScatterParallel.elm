module MainScatterParallel exposing (..)

import Browser
import Http
import Data
import Html exposing (Html, text)
import Html.Attributes exposing (href, value, target)
import Html.Events exposing (..)
import ParallelPlot
import Scatterplot
-- import full Plots and Data to explicitly show where the functions originate --

-- basic elm strucutre --
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
        { url = "https://raw.githubusercontent.com/Lena-Ar/Info-Vis/main/Daten/CSV/XboxOne_GameSales_Projekt.csv"
        , expect = Http.expectString GotText
        }
    )


-- definition of Model --
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


-- definition of Msg with all possible changes by interaction --
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


-- overwrites model according to changes -- 
-- essential for interactions --
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    -- initial overwriting with values for record fields in variant Success and use of decoder by using Data.gamesSalesList --
                    ( Success <| { data = Data.gamesSalesList [fullText], genre = "Action", plot = Data.ParallelPlot, axis1 = Data.NorthAmerica, axis2 = Data.Europe, axis3 = Data.Japan, axis4 = Data.RestOfWorld, axis5 = Data.Global, name1 = Data.regionTypeToString Data.NorthAmerica, name2 = Data.regionTypeToString Data.Europe, name3 = Data.regionTypeToString Data.Japan, name4 = Data.regionTypeToString Data.RestOfWorld, name5 = Data.regionTypeToString Data.Global, xaxis = Data.NorthAmerica, yaxis = Data.NorthAmerica }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        -- cased for overwriting/updating for all other variants defined in Msg --
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


-- definition of all needed buttons for interactions with program --

-- for selecting genre --
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


-- for selecting all five axes for ParallelPlot --
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


-- for selecting x- and y-axis in Scatterplot --
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


-- for selecting Plot --
buttonPlot : Html Msg
buttonPlot =
    Html.select
        [ onInput (\ry -> Data.stringToPlotType ry |> ChangePlot) ]
        [ Html.option [ value "ParallelPlot" ] [ Html.text "Parallel Coordinate Plot" ]
        , Html.option [ value "Scatterplot" ] [ Html.text "Scatterplot" ]
        , Html.option [value "Tree Hierarchy"] [Html.text "Tree Hierarchy"]
        ]

-- filter for attribute genre, applied in view --
-- based on code for exercise 4 --
filterGenre : List Data.GameSales -> String -> List Data.GameSales
filterGenre allGames genretype =
    List.filter (\c -> c.genre == genretype) allGames


-- transforms Model to Html Msg to show on website --
view : Model -> Html Msg
view model =
    case model of
        -- one case for each variant of Model --
        Error ->
            text "Opening the data for sales of games on XBoxOne failed"

        Loading ->
            text "Loading GameSales data..."
        
        Success fullText ->
            -- outer let-in construction with functions for application of filterGenre --
            let
                gameSalesData: List Data.GameSales
                gameSalesData = 
                    fullText.data
                
                number_games: Int
                number_games =
                    List.length gameSalesData
                
                gameSalesDataFiltered = 
                    filterGenre fullText.data fullText.genre

                number_games_genre: Int
                number_games_genre =  
                    List.length gameSalesDataFiltered

            in
            case fullText.plot of 
                -- one case for each variant of PlotType to be able to switch plots --
                Data.ParallelPlot -> 
                    -- inner let-in construction for each case of PlotType to be able to keep state of Model with chosen genre in all cases of PlotType --
                    let
                        -- application of assignmentAndReduce on unfiltered data --
                        gameSalesDataNull : List Data.GameSales
                        gameSalesDataNull = 
                            ParallelPlot.assignmentAndReduce gameSalesData
                
                        number_games_null : Int
                        number_games_null = 
                            List.length gameSalesDataNull

                        -- application of assignmentAndReduce on filtered data by genre --
                        -- need of application of gameSalesDataFiltered from outer let-in construction to keep state of model --
                        clearedGameSalesData : List Data.GameSales
                        clearedGameSalesData = 
                            ParallelPlot.assignmentAndReduce gameSalesDataFiltered
                
                        number_games_cleared : Int
                        number_games_cleared = 
                            List.length clearedGameSalesData

                        -- application of multiDimenData by giving it all needed data --
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
                        -- HTML especially for parallelPlot --
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
                                    [ Html.text ("This parallel coordinates plot shows the sales of video games in millions of units for XBox One sorted by selected genre.") 
                                    , Html.br [][]
                                    , Html.text ("One polyline in this plot represents one videogame. The intersection with each parallel axis shows the value of the attribute displayed here. You can easily identify patterns, clusters and outliers in selected genre over all regions of the world. Note that you can only see the correlations of one axis to the neighbouring two. Feel free to change the axes to inspect all.")
                                    , Html.br [][]
                                    , Html.br [][]
                                    , Html.text ("By hovering over the lines you are able to inspect specific information for each videogame inculding exact sales data for each region.")
                                    ]
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
                                        ]
                                    , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Please choose the region you want to display on the second axis with the adjacent buttons: ") 
                                        , buttonAxis2
                                        ]
                                    , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Please choose the region you want to display on the third axis with the adjacent buttons: ")
                                        , buttonAxis3
                                        ]
                                    , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Please choose the region you want to display on the fourth axis with the adjacent buttons: ")
                                        , buttonAxis4
                                        ]
                                    , Html.h4 [Html.Attributes.style "fontSize" "15px"]
                                        [ Html.text ("Please choose the region you want to display on the fifth axis with the adjacent buttons: ")
                                        , buttonAxis5
                                        ]
                                    ]
                                ]
                            , Html.h2 [Html.Attributes.style "fontSize" "20px", Html.Attributes.style "padding" "5px 5px 10px 10px", Html.Attributes.style "text-align" "center", Html.Attributes.style "color" "rgb(75, 128, 36, 0.75)"]
                                [Html.text ("Parallel Coordinates Plot for " ++ fullText.genre++ " with " ++ (Data.regionTypeToString fullText.axis1) ++ " as first axis, " ++ (Data.regionTypeToString fullText.axis2) ++ " as second axis, " ++ (Data.regionTypeToString fullText.axis3) ++ " as third axis, " ++ (Data.regionTypeToString fullText.axis4) ++ " as fourth axis and " ++ (Data.regionTypeToString fullText.axis5) ++ " as fifth axis.")]
                            -- plotting scatterplotParallel by giving it the needed data --
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
                    -- inner let-in construction for each case of PlotType to be able to keep state of Model with chosen genre in all cases of PlotType --
                    let
                        -- application of filterAndReduceGames on unfiltered data --
                        gameSalesDataNull =
                            Scatterplot.filterAndReduceGames (fullText.data)
            
                        number_clearedGames: Int
                        number_clearedGames = 
                            List.length gameSalesDataNull.data

                         -- application of filterAndReduceGames on filtered data by genre --
                         -- need of application of gameSalesDataFiltered from outer let-in construction to keep state of model --
                        gameSalesDataCleared = 
                            Scatterplot.filterAndReduceGames (gameSalesDataFiltered)
                        
                        -- application of regionFilter for x- and y-values by giving it filtered data by genre
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
                        -- HTML especially for scatterplot --
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
                            -- plotting scatterplot by giving it the needed data so all filters are finally used together --
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
                -- no inner let-in construction and application of functions on which filterGenre is applied from outer let-in as not needed --
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
                                        [buttonPlot]]
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