module ElementView exposing (elementView)

import Browser
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Iso3 exposing (iso3Codes)
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
            , Element.row [ Element.padding 5, Element.spacing 10, Element.width Element.fill ] [ plotView model, nationsSelectorView model ]
            , Element.row [ Element.padding 5, Element.spacing 10, Element.width Element.fill ] [ dateSelectorView model, nationAdderView model ]
            ]
        )


dateSelectorView : State -> Element Msg
dateSelectorView model =
    Element.column [ Element.padding 5, Element.spacing 10, Element.alignLeft ] [ fromYearSelectorView model, toYearSelectorView model ]


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
                , Element.alignRight
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


getNationName : Iso3.NationIso3 -> String
getNationName nation =
    iso3Codes
        |> List.filter (\{ iso3Code } -> iso3Code == nation)
        |> List.head
        |> Maybe.map .countryOrArea
        |> Maybe.withDefault "Unknown"


shortenName : String -> String
shortenName name =
    if String.length name <= 12 then
        name

    else
        String.slice 0 12 name ++ "..."


nationsSelectorView : State -> Element Msg
nationsSelectorView model =
    let
        remove : Iso3.NationIso3 -> Element Msg
        remove nation =
            Input.button
                [ Element.padding 5
                , Background.color lightGrey
                , Border.rounded 3
                , Element.alignRight
                ]
                { label = Element.text "✕"
                , onPress = Just <| RemoveNation nation
                }

        check : Iso3.NationIso3 -> String -> Element Msg
        check iso3Code nationName =
            Input.checkbox [ Element.alignLeft ]
                { onChange = \_ -> ToggleSelected iso3Code
                , icon = Input.defaultCheckbox
                , checked = model.selected |> List.member iso3Code
                , label = Input.labelRight [] (Element.text nationName)
                }

        rowStyle : List (Element.Attribute msg)
        rowStyle =
            [ Element.spacing 5, Element.width Element.fill, Element.padding 3 ]

        oneRow : ( Iso3.NationIso3, String ) -> Element Msg
        oneRow ( iso3Code, nationName ) =
            Element.row rowStyle [ check iso3Code (shortenName nationName), remove iso3Code ]

        rows : List (Element Msg)
        rows =
            model.graphData
                |> Dict.keys
                |> List.map (\iso3Code -> ( iso3Code, getNationName iso3Code ))
                |> List.sortBy (\( _, nationName ) -> nationName)
                |> List.map oneRow
    in
    Element.column [ Element.alignRight ] rows


plotView : State -> Element msg
plotView model =
    Element.el [ Element.alignLeft ] (dataToPlottable model |> plotData |> Element.html)
