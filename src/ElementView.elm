module ElementView exposing (elementView)

import Browser
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Iso3 exposing (NationIso3, iso3Codes)
import TemperatureChart exposing (Msg(..), State, dataToPlottable, earliest, fetchTemperatureData, init, latest, plotData, update)


initialLoads =
    [ "GBR", "ITA", "NOR", "USA" ] |> List.map fetchTemperatureData |> Cmd.batch


main =
    Browser.element
        { init = \() -> ( init, initialLoads )
        , update = update
        , view = \model -> Element.layout [] (elementView model)
        , subscriptions = \_ -> Sub.none
        }


elementView : State -> Element Msg
elementView model =
    Element.el []
        (Element.column []
            [ Element.el [ Element.centerX, Font.bold, Font.size 24, Font.family [ Font.monospace ] ] (Element.text "Yearly average temperature")
            , Element.row [ Element.padding 5, Element.spacing 10, Element.width Element.fill ] [ plotView model, plotNationsView model ]
            , Element.row [ Element.padding 5, Element.spacing 10, Element.width Element.fill ] [ dateSelectorView model, nationAdderView model ]
            ]
        )


dateSelectorView : State -> Element Msg
dateSelectorView model =
    Element.column [ Element.padding 5, Element.spacing 10 ] [ fromYearSelectorView model, toYearSelectorView model ]


lightGrey =
    Element.rgb255 192 192 192


sliderStyle =
    [ Element.height (Element.px 30)
    , Element.behindContent
        (Element.el
            [ Element.width Element.fill
            , Element.height (Element.px 2)
            , Element.centerY
            , Background.color lightGrey
            , Border.rounded 2
            ]
            Element.none
        )
    ]


fromYearSelectorView : State -> Element Msg
fromYearSelectorView model =
    let
        cfg =
            { onChange = \year -> ceiling year |> ChangeFrom
            , label =
                Input.labelAbove []
                    (Element.text "From year:")
            , min = toFloat earliest
            , max = toFloat model.toYear
            , value = toFloat model.fromYear
            , thumb = Input.defaultThumb
            , step = Just 10
            }
    in
    Input.slider sliderStyle cfg


toYearSelectorView : State -> Element Msg
toYearSelectorView model =
    let
        cfg =
            { onChange = \year -> ceiling year |> ChangeTo
            , label =
                Input.labelAbove []
                    (Element.text "To year:")
            , min = toFloat model.fromYear
            , max = toFloat latest
            , value = toFloat model.toYear
            , thumb = Input.defaultThumb
            , step = Just 10
            }
    in
    Input.slider sliderStyle cfg


blue =
    Element.rgb255 0 0 238


purple =
    Element.rgb255 238 0 238


nationAdderView : State -> Element Msg
nationAdderView model =
    let
        button =
            Input.button
                [ Element.padding 5
                , Background.color lightGrey
                , Border.rounded 3
                ]
                { label = Element.text "add"
                , onPress = Just Download
                }

        textInput =
            let
                current =
                    model.nationToDownload |> Maybe.withDefault ""
            in
            Input.text [ Border.rounded 3, Element.width Element.fill ]
                { onChange = \nation -> SetNationToDownload nation
                , text = current
                , placeholder = Nothing -- Just (Input.placeholder [] (Element.text "nation"))
                , label = Input.labelLeft [] (Element.text "Nation to add:")
                }
    in
    Element.row [ Element.spacing 5, Element.width Element.fill ]
        [ textInput
        , button
        ]


searchNationByName : String -> List NationIso3
searchNationByName nationName =
    iso3Codes
        |> List.filter (\code -> code.countryOrArea |> String.toLower |> String.startsWith (nationName |> String.toLower))
        |> List.map .iso3Code


plotNationsView : State -> Element Msg
plotNationsView model =
    let
        remove nation =
            Input.button
                [ Element.padding 5
                , Background.color lightGrey
                , Border.rounded 3
                ]
                { label = Element.text "remove"
                , onPress = Just <| RemoveNation nation
                }

        check nation =
            Input.checkbox []
                { onChange = \_ -> ToggleSelected nation
                , icon = Input.defaultCheckbox
                , checked = model.selected |> List.member nation
                , label = Input.labelRight [] (Element.text nation)
                }

        rows =
            model.graphData
                |> Dict.keys
                |> List.map (\nation -> Element.row [ Element.spacing 3, Element.width Element.fill ] [ check nation, remove nation ])
    in
    Element.column [] rows


plotView : State -> Element msg
plotView model =
    Element.el [] (dataToPlottable model |> plotData |> Element.html)
