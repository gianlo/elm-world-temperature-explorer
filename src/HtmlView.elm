module HtmlView exposing (plotData, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (checked, class, name, type_, value)
import Html.Events exposing (onClick, onInput, stopPropagationOn, targetValue)
import Iso3 exposing (NationIso3, iso3Codes)
import Json.Decode
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import TemperatureChart exposing (Model, Msg(..), Point, dataToPlottable, maximumYearRange)


checkbox : Bool -> msg -> String -> Html msg
checkbox isChecked msg name =
    label
        []
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]


view : Model -> Html Msg
view model =
    div [ class "temp-chart-view" ]
        [ dataToPlottable model |> plotData
        , nationSelector (Dict.keys model.graphData) model
        , yearSelector model
        , viewNationToAdd model
        , viewNationToRemove model
        ]


nationSelector : List NationIso3 -> Model -> Html Msg
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


yearSelector : Model -> Html Msg
yearSelector model =
    div [ class "temp-chart-year-selector" ]
        [ fromYearSelector model
        , toYearSelector model
        ]


fromYearSelector : Model -> Html Msg
fromYearSelector model =
    let
        changeYear : String -> Msg
        changeYear txt =
            ChangeFrom (String.toInt txt |> Maybe.withDefault model.fromYear)
    in
    div []
        [ p [] [ text "From year:" ]
        , select [ onInput changeYear ]
            (maximumYearRange
                |> List.filter (\year -> year <= model.toYear)
                |> List.map (\year -> option [ value (String.fromInt year) ] [ text (String.fromInt year) ])
            )
        ]


toYearSelector : Model -> Html Msg
toYearSelector model =
    let
        changeYear : String -> Msg
        changeYear txt =
            ChangeTo (String.toInt txt |> Maybe.withDefault model.toYear)
    in
    div []
        [ p [] [ text "To year:" ]
        , select [ onInput changeYear ]
            (maximumYearRange
                |> List.filter (\year -> year >= model.fromYear)
                |> List.reverse
                |> List.map (\year -> option [ value (String.fromInt year) ] [ text (String.fromInt year) ])
            )
        ]


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


viewNationToAdd : Model -> Html Msg
viewNationToAdd model =
    div []
        [ p [] [ text "Add another nation:" ]
        , select [ onInput SetNationToDownload ]
            (iso3Codes
                |> List.filter (\{ iso3Code } -> Dict.keys model.graphData |> List.member iso3Code |> not)
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


viewNationToRemove : Model -> Html Msg
viewNationToRemove model =
    div []
        [ p [] [ text "Remove nation:" ]
        , select [ onChange SetNationToRemove, name "nation" ]
            (iso3Codes
                |> List.filter (\{ iso3Code } -> Dict.keys model.graphData |> List.member iso3Code)
                |> List.map (\{ countryOrArea, iso3Code } -> option [ value iso3Code ] [ text countryOrArea ])
            )
        , button [ onClick Remove ] [ text "remove" ]
        ]
