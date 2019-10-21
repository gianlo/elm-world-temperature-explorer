module TemperatureChart exposing (Msg(..), State, dataToPlottable, earliest, fetchTemperatureData, init, latest, plotData, update, view)

import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn, targetValue)
import Http
import Iso3 exposing (Iso3Record, NationIso3, iso3Codes)
import Json.Decode exposing (Decoder, field, float, int, list)
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


type alias State =
    { fromYear : Year
    , toYear : Year
    , selected : List NationIso3
    , graphData : MultiData
    , nationToDownload : Maybe NationIso3
    , nationToRemove : Maybe NationIso3
    , nationLookUpResult : List Iso3Record
    }


init : State
init =
    { fromYear = earliest, toYear = latest, selected = [], graphData = Dict.empty, nationToDownload = Nothing, nationToRemove = Nothing, nationLookUpResult = [] }


view : State -> Html Msg
view uistate =
    div [ class "temp-chart-view" ]
        [ dataToPlottable uistate |> plotData
        , nationSelector (Dict.keys uistate.graphData) uistate
        , yearSelector uistate
        , viewNationToAdd uistate
        , viewNationToRemove uistate
        ]


update : Msg -> State -> ( State, Cmd Msg )
update msg uistate =
    case msg of
        ToggleSelected nation ->
            updateStateOnly (toggleSelected nation uistate)

        ChangeFrom year ->
            updateStateOnly { uistate | fromYear = year }

        ChangeTo year ->
            updateStateOnly { uistate | toYear = year }

        SetNationToDownload nation ->
            let
                matching : { a | countryOrArea : String } -> Bool
                matching { countryOrArea } =
                    String.startsWith (String.toLower nation) (String.toLower countryOrArea)

                found =
                    if String.length nation >= 3 then
                        iso3Codes |> List.filter matching

                    else
                        []
            in
            updateStateOnly { uistate | nationToDownload = Just nation, nationLookUpResult = found }

        Download ->
            case uistate.nationToDownload of
                Just nation ->
                    ( { uistate | nationToDownload = Nothing, nationLookUpResult = [] }, String.toUpper nation |> fetchTemperatureData )

                Nothing ->
                    updateStateOnly { uistate | nationToDownload = Nothing, nationLookUpResult = [] }

        SetNationToRemove nation ->
            updateStateOnly { uistate | nationToRemove = Just nation }

        Remove ->
            case uistate.nationToRemove of
                Just nation ->
                    updateStateOnly
                        { uistate
                            | selected = uistate.selected |> List.filter (\i -> i /= nation)
                            , graphData = uistate.graphData |> Dict.remove nation
                            , nationToRemove = Nothing
                        }

                Nothing ->
                    updateStateOnly { uistate | nationToRemove = Nothing }

        GotData result ->
            case result of
                Ok (GetResponse nation data) ->
                    updateStateOnly { uistate | selected = nation :: uistate.selected, graphData = Dict.insert nation data uistate.graphData }

                Err error ->
                    let
                        errorReason =
                            error |> encodeError
                    in
                    updateStateOnly (log errorReason uistate)

        RemoveNation nation ->
            updateStateOnly
                { uistate
                    | selected = uistate.selected |> List.filter (\i -> i /= nation)
                    , graphData = uistate.graphData |> Dict.remove nation
                    , nationToRemove = Nothing
                }


dataUrl : String
dataUrl =
    "http://climatedataapi.worldbank.org/climateweb/rest/v1/country/cru/tas/year/"


type GetResponse
    = GetResponse NationIso3 Data


encodeError error =
    case error of
        Http.BadUrl url ->
            "BadUrl: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus code ->
            "BadStatus: " ++ String.fromInt code

        Http.BadBody body ->
            "BadBody: " ++ body


fetchTemperatureData : NationIso3 -> Cmd Msg
fetchTemperatureData nation =
    let
        transform : Result Http.Error Data -> Msg
        transform result =
            GotData (Result.map (\data -> GetResponse nation data) result)
    in
    Http.get
        { url = dataUrl ++ nation
        , expect = Http.expectJson transform dataDecoder
        }


updateStateOnly : State -> ( State, Cmd msg )
updateStateOnly s =
    ( s, Cmd.none )


type Msg
    = ToggleSelected NationIso3
    | ChangeFrom Year
    | ChangeTo Year
    | SetNationToDownload NationIso3
    | Download
    | SetNationToRemove NationIso3
    | RemoveNation NationIso3
    | Remove
    | GotData (Result Http.Error GetResponse)


earliest : Year
earliest =
    1900


latest : Year
latest =
    2030


type alias Celsius =
    Float


type alias AnnualTemperature =
    { year : Year
    , temp : Celsius
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
        getNationName nation =
            iso3Codes
                |> List.filter (\{ iso3Code } -> iso3Code == nation)
                |> List.head
                |> Maybe.map .countryOrArea
                |> Maybe.withDefault "Unknown"

        ( isoCodes, nationNames ) =
            all
                |> List.map (\iso3Code -> ( iso3Code, getNationName iso3Code ))
                |> List.sortBy (\( _, nationName ) -> nationName)
                |> List.unzip

        shortenName : String -> String
        shortenName name =
            if String.length name <= 12 then
                name

            else
                String.slice 0 12 name ++ "..."

        theCheckbox : NationIso3 -> String -> Html Msg
        theCheckbox iso3Code nation =
            checkbox (selected |> List.member iso3Code) (ToggleSelected iso3Code) (shortenName nation ++ " (" ++ iso3Code ++ ")")
    in
    div [ class "temp-chart-nation-selector" ] [ fieldset [] (List.map2 theCheckbox isoCodes nationNames) ]


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


computeSpatialAverage : List ( NationIso3, Data ) -> Data
computeSpatialAverage selectedTimeSeries =
    let
        -- extract only time series
        ( _, series ) =
            selectedTimeSeries |> List.unzip

        -- flatten all the timeseries in a big one :-)
        flattened : Data
        flattened =
            series |> List.concat

        appendIfExisting : Celsius -> Maybe (List Celsius) -> Maybe (List Celsius)
        appendIfExisting temp current =
            case current of
                Just vs ->
                    Just (temp :: vs)

                Nothing ->
                    Just [ temp ]

        aggregationFunc : AnnualTemperature -> Dict Year (List Celsius) -> Dict Year (List Celsius)
        aggregationFunc value acc =
            Dict.update value.year (appendIfExisting value.temp) acc

        -- collect for each year all relevant temp measurements
        aggregated : Dict Year (List Celsius)
        aggregated =
            flattened
                |> List.foldl aggregationFunc Dict.empty

        sampleMean : List Celsius -> Celsius
        sampleMean temps =
            List.sum temps / toFloat (List.length temps)

        -- compute the sample mean temp for each year
        spatialAverage : Data
        spatialAverage =
            aggregated
                |> Dict.toList
                |> List.map (\( year, temps ) -> { year = year, temp = sampleMean temps })
    in
    spatialAverage


dataToPlottable : State -> List (LineChart.Series Point)
dataToPlottable uistate =
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
            in
            LineChart.line color Dots.square nation (toDataPoints uistate data)

        selectedTimeSeries : List ( NationIso3, Data )
        selectedTimeSeries =
            Dict.toList uistate.graphData
                |> List.filter (\( nation, _ ) -> uistate.selected |> List.member nation)

        spatialAverageTimeSeries =
            if List.length selectedTimeSeries > 1 then
                [ ( "MEAN", computeSpatialAverage selectedTimeSeries ) ]

            else
                []
    in
    selectedTimeSeries
        ++ spatialAverageTimeSeries
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


viewNationToAdd : State -> Html Msg
viewNationToAdd uistate =
    div []
        [ p [] [ text "Add another nation:" ]
        , select [ onInput SetNationToDownload ]
            (iso3Codes
                |> List.filter (\{ iso3Code } -> Dict.keys uistate.graphData |> List.member iso3Code |> not)
                |> List.map (\{ countryOrArea, iso3Code } -> option [ value iso3Code ] [ text countryOrArea ])
            )
        , button [ onClick Download ] [ text "add" ]
        ]


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    stopPropagationOn "change" <|
        Json.Decode.map alwaysStop (Json.Decode.map tagger targetValue)


alwaysStop : a -> ( a, Bool )
alwaysStop x =
    ( x, True )


viewNationToRemove : State -> Html Msg
viewNationToRemove uistate =
    div []
        [ p [] [ text "Remove nation:" ]
        , select [ onChange SetNationToRemove, name "nation" ]
            (iso3Codes
                |> List.filter (\{ iso3Code } -> Dict.keys uistate.graphData |> List.member iso3Code)
                |> List.map (\{ countryOrArea, iso3Code } -> option [ value iso3Code ] [ text countryOrArea ])
            )
        , button [ onClick Remove ] [ text "remove" ]
        ]
