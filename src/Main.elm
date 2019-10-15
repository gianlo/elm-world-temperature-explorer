module Main exposing (main)

import Browser
import Dict exposing (Dict, insert)
import Html exposing (..)
import Html.Attributes exposing (checked, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso3
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
    = Loading (List NationIso3) MultiData UIState
    | Failure String
    | Complete MultiData UIState


type alias UIState =
    { fromYear : Year, toYear : Year, selected : List NationIso3 }


type alias Data =
    List AnnualTemperature


type alias NationIso3 =
    String


type alias MultiData =
    Dict NationIso3 Data


type alias Year =
    Int


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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading [ "NOR", "ITA", "DZA", "ZAF" ] Dict.empty (UIState earliest latest [ "GBR" ]), fetchTemperatureData "GBR" )


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
    | ChangeFrom Year
    | ChangeTo Year
    | ToggleSelected NationIso3


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok (GetResponse nation data) ->
                    case model of
                        Loading [] multidata uistate ->
                            ( Complete (Dict.insert nation data multidata) { uistate | selected = nation :: uistate.selected }, Cmd.none )

                        Loading (nextNation :: tail) multidata uistate ->
                            ( Loading tail (Dict.insert nation data multidata) { uistate | selected = nation :: uistate.selected }, fetchTemperatureData nextNation )

                        _ ->
                            ( model, Cmd.none )

                Err error ->
                    error |> encodeError |> errorEncoding

        ChangeFrom year ->
            case model of
                Loading nations multidata uistate ->
                    ( Loading nations multidata { uistate | fromYear = year }, Cmd.none )

                Complete multidata uistate ->
                    ( Complete multidata { uistate | fromYear = year }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeTo year ->
            case model of
                Loading nations multidata uistate ->
                    ( Loading nations multidata { uistate | toYear = year }, Cmd.none )

                Complete multidata uistate ->
                    ( Complete multidata { uistate | toYear = year }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ToggleSelected nation ->
            case model of
                Loading nations multidata uistate ->
                    ( Loading nations multidata (toggleSelected nation uistate), Cmd.none )

                Complete multidata uistate ->
                    ( Complete multidata (toggleSelected nation uistate), Cmd.none )

                _ ->
                    ( model, Cmd.none )


toggleSelected : NationIso3 -> Selected m -> Selected m
toggleSelected nation selection =
    if selection.selected |> List.member nation then
        { selection | selected = selection.selected |> List.filter (\n -> n /= nation) }

    else
        { selection | selected = nation :: selection.selected }


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


view : Model -> Html Msg
view model =
    case model of
        Loading nations multidata uistate ->
            div []
                [ text ("still loading: " ++ String.join ", " nations)
                , plotData uistate multidata
                , nationSelector (Dict.keys multidata) uistate
                , yearSelector uistate
                , nationToAddSelector
                ]

        Failure error ->
            div []
                [ text ("failed: " ++ error)
                ]

        Complete multidata uistate ->
            div []
                [ text "Here's the Climate Data"
                , plotData uistate multidata
                , nationSelector (Dict.keys multidata) uistate
                , yearSelector uistate
                , nationToAddSelector
                ]


type alias Selected m =
    { m | selected : List NationIso3 }


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
    fieldset [] (all |> List.map theCheckbox)


type alias YearRange m =
    { m | fromYear : Year, toYear : Year }


maximumYearRange : List Year
maximumYearRange =
    List.range 0 13
        |> List.map (\decade -> earliest + 10 * decade)


yearSelector : YearRange m -> Html Msg
yearSelector uistate =
    div []
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


plotData : UIState -> MultiData -> Html msg
plotData uistate multidata =
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
        (Dict.toList multidata
            |> List.filter (\( nation, data ) -> uistate.selected |> List.member nation)
            |> List.map (\( nation, data ) -> aChart nation data)
        )


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
            (Iso3.iso3Codes
                |> List.map (\{ countryOrArea, iso3Code } -> option [ value iso3Code ] [ text countryOrArea ])
            )
        ]
