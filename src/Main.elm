module Main exposing (main)

import Browser
import Dict exposing (Dict, insert)
import Html exposing (..)
import Html.Attributes exposing (checked, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso3 exposing (NationIso3)
import Json.Decode exposing (Decoder, field, float, int, list, string)
import TemperatureChart


type Model
    = Loading (List NationIso3) UIState
    | Failure String
    | Complete UIState


type alias UIState =
    TemperatureChart.State


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading [ "NOR", "ITA", "DZA", "ZAF" ] TemperatureChart.init, fetchTemperatureData "GBR" )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


type GetResponse
    = GetResponse NationIso3 TemperatureChart.Data


type Msg
    = GotData (Result Http.Error GetResponse)
    | TemperatureChartMsg TemperatureChart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok (GetResponse nation data) ->
                    let
                        updateState uistate =
                            { uistate | selected = nation :: uistate.selected, graphData = Dict.insert nation data uistate.graphData }
                    in
                    case model of
                        Loading [] uistate ->
                            ( Complete (updateState uistate), Cmd.none )

                        Loading (nextNation :: tail) uistate ->
                            ( Loading tail (updateState uistate), fetchTemperatureData nextNation )

                        Complete uistate ->
                            let
                                newState =
                                    updateState uistate
                            in
                            ( Complete newState, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err error ->
                    error |> encodeError |> errorEncoding

        TemperatureChartMsg tempCharMsg ->
            case model of
                Loading nations uistate ->
                    let
                        ( newUistate, cmd ) =
                            TemperatureChart.update fetchTemperatureData tempCharMsg uistate
                    in
                    ( Loading nations newUistate, cmd )

                Complete uistate ->
                    let
                        ( newUistate, cmd ) =
                            TemperatureChart.update fetchTemperatureData tempCharMsg uistate
                    in
                    ( Complete newUistate, cmd )

                _ ->
                    ( model, Cmd.none )


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
        transform : Result Http.Error TemperatureChart.Data -> Msg
        transform result =
            GotData (Result.map (\data -> GetResponse nation data) result)
    in
    Http.get
        { url = dataUrl ++ nation
        , expect = Http.expectJson transform TemperatureChart.dataDecoder
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Loading nations uistate ->
            div []
                [ text ("still loading: " ++ String.join ", " nations)
                , TemperatureChart.view uistate |> Html.map TemperatureChartMsg
                ]

        Failure error ->
            div []
                [ text ("failed: " ++ error)
                ]

        Complete uistate ->
            div []
                [ text "Here's the Climate Data"
                , TemperatureChart.view uistate |> Html.map TemperatureChartMsg
                ]
