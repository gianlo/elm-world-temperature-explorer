module TemperatureChart exposing (Model, Msg(..), Point, dataToPlottable, earliest, fetchTemperatureData, init, latest, maximumYearRange, update)

import Dict exposing (Dict)
import Http
import Iso3 exposing (Iso3Record, NationIso3, iso3Codes)
import Json.Decode exposing (Decoder, field, float, int, list)
import LineChart
import LineChart.Colors as Colors
import LineChart.Dots as Dots


type alias Model =
    { fromYear : Year
    , toYear : Year
    , selected : List NationIso3
    , graphData : MultiData
    , nationToDownload : Maybe NationIso3
    , nationToRemove : Maybe NationIso3
    , nationLookUpResult : List Iso3Record
    }


init : Model
init =
    { fromYear = earliest, toYear = latest, selected = [], graphData = Dict.empty, nationToDownload = Nothing, nationToRemove = Nothing, nationLookUpResult = [] }


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
    | DownloadThis NationIso3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleSelected nation ->
            updateStateOnly (toggleSelected nation model)

        ChangeFrom year ->
            updateStateOnly { model | fromYear = year }

        ChangeTo year ->
            updateStateOnly { model | toYear = year }

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
            updateStateOnly { model | nationToDownload = Just nation, nationLookUpResult = found }

        Download ->
            case model.nationToDownload of
                Just nation ->
                    ( { model | nationToDownload = Nothing, nationLookUpResult = [] }, String.toUpper nation |> fetchTemperatureData )

                Nothing ->
                    updateStateOnly { model | nationToDownload = Nothing, nationLookUpResult = [] }

        DownloadThis nation ->
            ( { model | nationToDownload = Nothing, nationLookUpResult = [] }, String.toUpper nation |> fetchTemperatureData )

        SetNationToRemove nation ->
            updateStateOnly { model | nationToRemove = Just nation }

        Remove ->
            case model.nationToRemove of
                Just nation ->
                    updateStateOnly
                        { model
                            | selected = model.selected |> List.filter (\i -> i /= nation)
                            , graphData = model.graphData |> Dict.remove nation
                            , nationToRemove = Nothing
                        }

                Nothing ->
                    updateStateOnly { model | nationToRemove = Nothing }

        GotData result ->
            case result of
                Ok (GetResponse nation data) ->
                    updateStateOnly { model | selected = nation :: model.selected, graphData = Dict.insert nation data model.graphData }

                _ ->
                    updateStateOnly model

        RemoveNation nation ->
            updateStateOnly
                { model
                    | selected = model.selected |> List.filter (\i -> i /= nation)
                    , graphData = model.graphData |> Dict.remove nation
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


updateStateOnly : Model -> ( Model, Cmd msg )
updateStateOnly s =
    ( s, Cmd.none )


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


toggleSelected : NationIso3 -> Model -> Model
toggleSelected nation model =
    if model.selected |> List.member nation then
        { model | selected = model.selected |> List.filter (\n -> n /= nation) }

    else
        { model | selected = nation :: model.selected }


dataDecoder : Decoder Data
dataDecoder =
    list annualTemperatureDecoder


annualTemperatureDecoder : Decoder AnnualTemperature
annualTemperatureDecoder =
    Json.Decode.map2 AnnualTemperature (field "year" int) (field "data" float)


maximumYearRange : List Year
maximumYearRange =
    List.range 0 13
        |> List.map (\decade -> earliest + 10 * decade)


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


dataToPlottable : Model -> List (LineChart.Series Point)
dataToPlottable model =
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
            LineChart.line color Dots.square nation (toDataPoints model data)

        selectedTimeSeries : List ( NationIso3, Data )
        selectedTimeSeries =
            Dict.toList model.graphData
                |> List.filter (\( nation, _ ) -> model.selected |> List.member nation)

        spatialAverageTimeSeries =
            if List.length selectedTimeSeries > 1 then
                [ ( "MEAN", computeSpatialAverage selectedTimeSeries ) ]

            else
                []
    in
    selectedTimeSeries
        ++ spatialAverageTimeSeries
        |> List.map (\( nation, data ) -> aChart nation data)


type alias Point =
    { x : Float, y : Float }


toDataPoints : Model -> Data -> List Point
toDataPoints model data =
    List.filter (\p -> p.x >= toFloat model.fromYear && p.x <= toFloat model.toYear) (List.map (\d -> Point (toFloat d.year) d.temp) data)
