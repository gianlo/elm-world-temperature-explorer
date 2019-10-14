module Main exposing (main)

import Browser
import Dict exposing (Dict, insert)
import Html exposing (..)
import Http
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


type Model
    = Loading (List NationIso3) MultiData
    | Failure String
    | Complete MultiData


type alias Data =
    List AnnualTemperature


type alias NationIso3 =
    String


type alias MultiData =
    Dict NationIso3 Data


type alias AnnualTemperature =
    { year : Int
    , temp : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading [ "GBR", "DZA", "ZAF" ] Dict.empty, fetchTemperatureData "ITA" )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


type GetResponse
    = GetResponse NationIso3 Data


type Msg
    = GotData (Result Http.Error GetResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok (GetResponse nation data) ->
                    case model of
                        Loading [] multidata ->
                            ( Complete multidata, Cmd.none )

                        Loading (nextNation :: tail) multidata ->
                            ( Loading tail (Dict.insert nation data multidata), fetchTemperatureData nextNation )

                        _ ->
                            ( model, Cmd.none )

                Err error ->
                    error |> encodeError |> errorEncoding


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


errorEncoding : String -> ( Model, Cmd msg )
errorEncoding txt =
    ( Failure txt, Cmd.none )


dataUrl : String
dataUrl =
    "http://climatedataapi.worldbank.org/climateweb/rest/v1/country/cru/tas/year/"


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


dataDecoder : Decoder Data
dataDecoder =
    list annualTemperatureDecoder


annualTemperatureDecoder : Decoder AnnualTemperature
annualTemperatureDecoder =
    Json.Decode.map2 AnnualTemperature (field "year" int) (field "data" float)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html msg
view model =
    case model of
        Loading nations _ ->
            div []
                [ text ("loading: " ++ String.join ", " nations)
                ]

        Failure error ->
            div []
                [ text ("failed: " ++ error)
                ]

        Complete multidata ->
            div []
                [ text "Here's the data"
                , plotData multidata
                ]


plotData : MultiData -> Html msg
plotData multidata =
    let
        colors =
            Dict.fromList [ ( 0, Colors.blue ), ( 1, Colors.red ), ( 2, Colors.green ), ( 3, Colors.black ), ( 4, Colors.gray ) ]

        aChart : Int -> NationIso3 -> Data -> LineChart.Series Point
        aChart colorIndex nation data =
            let
                color =
                    Dict.get (modBy (Dict.size colors) colorIndex) colors |> Maybe.withDefault Colors.black
            in
            LineChart.line color Dots.square nation (toDataPoints data)
    in
    LineChart.viewCustom
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
        (Dict.toList multidata |> List.indexedMap (\colorIndex ( nation, data ) -> aChart colorIndex nation data))


type alias Point =
    { x : Float, y : Float }


toDataPoints : Data -> List Point
toDataPoints data =
    List.map (\d -> Point (toFloat d.year) d.temp) data
