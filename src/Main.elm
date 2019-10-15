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
    | Complete UIState


type alias UIState =
    TemperatureChart.State


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading [ "NOR", "ITA", "DZA", "ZAF" ] TemperatureChart.init, TemperatureChart.fetchTemperatureData "GBR" |> Cmd.map TemperatureChartMsg )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UPDATE


type Msg
    = TemperatureChartMsg TemperatureChart.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TemperatureChartMsg tempCharMsg ->
            case model of
                Loading [] uistate ->
                    liftTemperatureChartUpdate tempCharMsg uistate Complete []

                Loading (nation :: others) uistate ->
                    liftTemperatureChartUpdate tempCharMsg uistate (Loading others) [ TemperatureChart.fetchTemperatureData nation ]

                Complete uistate ->
                    liftTemperatureChartUpdate tempCharMsg uistate Complete []


liftTemperatureChartUpdate : TemperatureChart.Msg -> TemperatureChart.State -> (TemperatureChart.State -> Model) -> List (Cmd TemperatureChart.Msg) -> ( Model, Cmd Msg )
liftTemperatureChartUpdate msg uistate transition extraCmds =
    let
        ( newUistate, cmd ) =
            TemperatureChart.update msg uistate
    in
    ( transition newUistate, [ cmd ] ++ extraCmds |> Cmd.batch |> Cmd.map TemperatureChartMsg )



-- updateStateOnly : mdl -> ( mdl, Cmd msg )
-- updateStateOnly model =
--     ( model, Cmd.none )
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

        Complete uistate ->
            div []
                [ text "Here's the Climate Data"
                , TemperatureChart.view uistate |> Html.map TemperatureChartMsg
                ]
