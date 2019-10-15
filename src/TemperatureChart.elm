module TemperatureChart exposing (Data, Msg, MultiData, State, Year, dataDecoder, init, update, view)

import Dict exposing (Dict, insert)
import Html exposing (..)
import Html.Attributes exposing (checked, class, type_, value)
import Html.Events exposing (onClick, onInput)
import Iso3 exposing (NationIso3, iso3Codes)
import Json.Decode exposing (Decoder, field, float, int, list, string)
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line


init : State
init =
    { fromYear = earliest, toYear = latest, selected = [], graphData = Dict.empty }


view : State -> Html Msg
view uistate =
    div [ class "temp-chart-view" ]
        [ dataToPlottable uistate uistate.graphData |> plotData
        , nationSelector (Dict.keys uistate.graphData) uistate
        , yearSelector uistate
        , nationToAddSelector
        ]


type alias State =
    { fromYear : Year, toYear : Year, selected : List NationIso3, graphData : MultiData }


type Msg
    = ToggleSelected NationIso3
    | ChangeFrom Year
    | ChangeTo Year


update : Msg -> State -> State
update msg uistate =
    case msg of
        ToggleSelected nation ->
            toggleSelected nation uistate

        ChangeFrom year ->
            { uistate | fromYear = year }

        ChangeTo year ->
            { uistate | toYear = year }


earliest : Year
earliest =
    1900


latest : Year
latest =
    2030


type alias AnnualTemperature =
    { year : Int
    , temp : Float
    }


type alias Data =
    List AnnualTemperature


type alias MultiData =
    Dict NationIso3 Data


type alias Year =
    Int


type alias Selected m =
    { m | selected : List NationIso3 }


type alias YearRange m =
    { m | fromYear : Year, toYear : Year }


toggleSelected : NationIso3 -> Selected m -> Selected m
toggleSelected nation selection =
    if selection.selected |> List.member nation then
        { selection | selected = selection.selected |> List.filter (\n -> n /= nation) }

    else
        { selection | selected = nation :: selection.selected }


dataDecoder : Decoder Data
dataDecoder =
    list annualTemperatureDecoder


annualTemperatureDecoder : Decoder AnnualTemperature
annualTemperatureDecoder =
    Json.Decode.map2 AnnualTemperature (field "year" int) (field "data" float)


checkbox : Bool -> msg -> String -> Html msg
checkbox isChecked msg name =
    label
        []
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]


nationSelector : List NationIso3 -> Selected m -> Html Msg
nationSelector all { selected } =
    let
        theCheckbox : NationIso3 -> Html Msg
        theCheckbox nation =
            checkbox (selected |> List.member nation) (ToggleSelected nation) nation
    in
    div [ class "temp-chart-nation-selector" ] [ fieldset [] (all |> List.map theCheckbox) ]


maximumYearRange : List Year
maximumYearRange =
    List.range 0 13
        |> List.map (\decade -> earliest + 10 * decade)


yearSelector : YearRange m -> Html Msg
yearSelector uistate =
    div [ class "temp-chart-year-selector" ]
        [ fromYearSelector uistate
        , toYearSelector uistate
        ]


fromYearSelector : YearRange m -> Html Msg
fromYearSelector uistate =
    let
        changeYear : String -> Msg
        changeYear txt =
            ChangeFrom (String.toInt txt |> Maybe.withDefault uistate.fromYear)
    in
    div []
        [ p [] [ text "From year:" ]
        , select [ onInput changeYear ]
            (maximumYearRange
                |> List.filter (\year -> year <= uistate.toYear)
                |> List.map (\year -> option [ value (String.fromInt year) ] [ text (String.fromInt year) ])
            )
        ]


toYearSelector : YearRange m -> Html Msg
toYearSelector uistate =
    let
        changeYear : String -> Msg
        changeYear txt =
            ChangeTo (String.toInt txt |> Maybe.withDefault uistate.toYear)
    in
    div []
        [ p [] [ text "To year:" ]
        , select [ onInput changeYear ]
            (maximumYearRange
                |> List.filter (\year -> year >= uistate.fromYear)
                |> List.reverse
                |> List.map (\year -> option [ value (String.fromInt year) ] [ text (String.fromInt year) ])
            )
        ]


type alias ThreeLetters =
    ( Char, Char, Char )


stringToThreeLetters : String -> ThreeLetters
stringToThreeLetters txt =
    case txt |> String.toList of
        first :: second :: third :: _ ->
            ( first, second, third )

        first :: second :: [] ->
            ( first, second, ' ' )

        first :: [] ->
            ( first, ' ', ' ' )

        _ ->
            ( ' ', ' ', ' ' )


simpleHash : ThreeLetters -> Int
simpleHash ( first, second, third ) =
    let
        hundreds =
            65536

        tens =
            256
    in
    [ ( first, 1 ), ( second, tens ), ( third, hundreds ) ]
        |> List.map (\( ch, base ) -> base * Char.toCode ch)
        |> List.foldl (+) 0


dataToPlottable : State -> MultiData -> List (LineChart.Series Point)
dataToPlottable uistate multidata =
    let
        colors =
            Dict.fromList
                [ ( 0, Colors.blue )
                , ( 1, Colors.red )
                , ( 2, Colors.green )
                , ( 3, Colors.black )
                , ( 4, Colors.gray )
                , ( 5, Colors.gold )
                , ( 6, Colors.pink )
                , ( 7, Colors.purple )
                , ( 8, Colors.rust )
                , ( 9, Colors.teal )
                , ( 10, Colors.strongBlue )
                ]

        aChart : NationIso3 -> Data -> LineChart.Series Point
        aChart nation data =
            let
                modIndex =
                    stringToThreeLetters nation
                        |> simpleHash
                        |> modBy (Dict.size colors)

                color =
                    Dict.get modIndex colors |> Maybe.withDefault Colors.black

                dateFilter : Data -> Data
                dateFilter ds =
                    ds |> List.filter (\{ year } -> year >= uistate.fromYear && year <= uistate.toYear)
            in
            LineChart.line color Dots.square nation (toDataPoints uistate data)
    in
    Dict.toList multidata
        |> List.filter (\( nation, data ) -> uistate.selected |> List.member nation)
        |> List.map (\( nation, data ) -> aChart nation data)


plotData : List (LineChart.Series Point) -> Html msg
plotData series =
    div [ class "temp-chart" ]
        [ LineChart.viewCustom
            { x = Axis.default 700 "year" .x
            , y = Axis.default 400 "temp (degree)" .y
            , container = Container.default "line-chart-1"
            , interpolation = Interpolation.default
            , intersection = Intersection.default
            , legends = Legends.default
            , events = Events.default
            , junk = Junk.default
            , grid = Grid.default
            , area = Area.default
            , line = Line.default
            , dots = Dots.default
            }
            series
        ]


type alias Point =
    { x : Float, y : Float }


toDataPoints : YearRange m -> Data -> List Point
toDataPoints uistate data =
    List.filter (\p -> p.x >= toFloat uistate.fromYear && p.x <= toFloat uistate.toYear) (List.map (\d -> Point (toFloat d.year) d.temp) data)


nationToAddSelector : Html msg
nationToAddSelector =
    div []
        [ p [] [ text "Nation:" ]
        , select []
            (iso3Codes
                |> List.map (\{ countryOrArea, iso3Code } -> option [ value iso3Code ] [ text countryOrArea ])
            )
        ]