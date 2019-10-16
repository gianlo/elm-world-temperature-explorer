module Main exposing (main)

import Browser
import Dict exposing (Dict, insert)
import Html exposing (..)
import Html.Attributes exposing (checked, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso3 exposing (NationIso3, iso3Codes)
import Json.Decode exposing (Decoder, field, float, int, list, string)
import Random
import TemperatureChart


type Model
    = Model UIState


type alias UIState =
    TemperatureChart.State


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model TemperatureChart.init, [ loadNation "GBR", pickRandomCountry 4 ] |> Cmd.batch )


loadNation : NationIso3 -> Cmd Msg
loadNation nation =
    TemperatureChart.fetchTemperatureData nation |> Cmd.map TemperatureChartMsg


getNationFromIndex : Int -> NationIso3
getNationFromIndex n =
    case iso3Codes |> List.drop (n - 1) of
        head :: _ ->
            head.iso3Code

        _ ->
            "GBR"


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
    | GetSampleNation Int Int


numberOfCountries : Int
numberOfCountries =
    iso3Codes |> List.length


pickRandomCountry : Int -> Cmd Msg
pickRandomCountry remaining =
    Random.generate (GetSampleNation remaining) (Random.int 0 (numberOfCountries - 1))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TemperatureChartMsg tempCharMsg ->
            case model of
                Model uistate ->
                    liftTemperatureChartUpdate tempCharMsg uistate Model []

        GetSampleNation remaining nationIndex ->
            case model of
                Model uistate ->
                    let
                        newCountry =
                            getNationFromIndex nationIndex

                        loadNewNation =
                            loadNation newCountry
                    in
                    if List.member newCountry (Dict.keys uistate.graphData) then
                        -- country already loaded, sample another one
                        ( Model uistate, pickRandomCountry remaining )

                    else if remaining > 1 then
                        -- pick another country and load this one
                        ( Model uistate, [ pickRandomCountry (remaining - 1), loadNewNation ] |> Cmd.batch )

                    else
                        -- load this one
                        ( Model uistate, loadNewNation )


liftTemperatureChartUpdate : TemperatureChart.Msg -> TemperatureChart.State -> (TemperatureChart.State -> Model) -> List (Cmd TemperatureChart.Msg) -> ( Model, Cmd Msg )
liftTemperatureChartUpdate msg uistate transition extraCmds =
    let
        ( newUistate, cmd ) =
            TemperatureChart.update msg uistate
    in
    ( transition newUistate, [ cmd ] ++ extraCmds |> Cmd.batch |> Cmd.map TemperatureChartMsg )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Model uistate ->
            div []
                [ text "Here's the Climate Data"
                , TemperatureChart.view uistate |> Html.map TemperatureChartMsg
                ]
