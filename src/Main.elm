module Main exposing (main)

import Browser
import Dict
import Html exposing (..)
import Http
import Iso3 exposing (NationIso3, iso3Codes)
import Random
import TemperatureChart


type alias Model =
    TemperatureChart.State


init : () -> ( Model, Cmd Msg )
init _ =
    ( TemperatureChart.init, [ loadNation "GBR", pickRandomCountry 4 ] |> Cmd.batch )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TemperatureChartMsg tempCharMsg ->
            TemperatureChart.update tempCharMsg model
                |> Tuple.mapSecond (Cmd.map TemperatureChartMsg)

        GetSampleNation remaining nationIndex ->
            let
                newCountry =
                    getNationFromIndex nationIndex

                loadNewNation =
                    loadNation newCountry
            in
            if List.member newCountry (Dict.keys model.graphData) then
                -- country already loaded, sample another one
                ( model, pickRandomCountry remaining )

            else if remaining > 1 then
                -- pick another country and load this one
                ( model, [ pickRandomCountry (remaining - 1), loadNewNation ] |> Cmd.batch )

            else
                -- load this one
                ( model, loadNewNation )


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


numberOfCountries : Int
numberOfCountries =
    iso3Codes |> List.length


pickRandomCountry : Int -> Cmd Msg
pickRandomCountry remaining =
    Random.generate (GetSampleNation remaining) (Random.int 0 (numberOfCountries - 1))



-- SUBSCRIPTIONS


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Here's the Climate Data"
        , TemperatureChart.view model |> Html.map TemperatureChartMsg
        ]
