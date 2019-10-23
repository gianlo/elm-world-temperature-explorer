module ElmUiView exposing (elementView)

import Browser
import Dict
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import HtmlView exposing (plotData)
import Iso3 exposing (iso3Codes)
import TemperatureChart exposing (Model, Msg(..), dataToPlottable, earliest, fetchTemperatureData, init, latest, update)


initialLoads : Cmd Msg
initialLoads =
    [ "GBR", "ITA", "NOR", "USA" ] |> List.map fetchTemperatureData |> Cmd.batch


main =
    Browser.element
        { init = \() -> ( init, initialLoads )
        , update = update
        , view = \model -> Element.layout [] (elementView model)
        , subscriptions = \_ -> Sub.none
        }


elementView : Model -> Element Msg
elementView model =
    Element.el []
        (Element.column []
            [ Element.el [ Element.centerX, Font.bold, Font.size 24, Font.family [ Font.monospace ] ] (Element.text "Yearly average temperature")
            , Element.row [ Element.padding 5, Element.spacing 10, Element.width Element.fill ] [ plotView model, nationsSelectorView model ]
            , Element.row [ Element.padding 5, Element.spacing 10, Element.width Element.fill ] [ dateSelectorView model, nationAdderView model ]
            ]
        )


dateSelectorView : Model -> Element Msg
dateSelectorView model =
    Element.column [ Element.padding 5, Element.spacing 10, Element.alignLeft ] [ fromYearSelectorView model, toYearSelectorView model ]


lightGrey : Element.Color
lightGrey =
    Element.rgb255 192 192 192


sliderStyle : List (Element.Attribute msg)
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


fromYearSelectorView : Model -> Element Msg
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


toYearSelectorView : Model -> Element Msg
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


nationAdderView : Model -> Element Msg
nationAdderView model =
    let
        downloadButton nationName iso3Code =
            Input.button
                [ Element.padding 5
                , Background.color lightGrey
                , Border.rounded 3
                , Element.alignRight
                ]
                { label = Element.text nationName
                , onPress = Just (DownloadThis iso3Code)
                }

        withTip =
            if List.length model.nationLookUpResult > 0 then
                Element.el [ Element.onRight (Element.column [] (model.nationLookUpResult |> List.map (\{ countryOrArea, iso3Code } -> downloadButton countryOrArea iso3Code))) ] textInput

            else
                textInput

        textInput =
            let
                current =
                    model.nationToDownload |> Maybe.withDefault ""
            in
            Input.text [ Border.rounded 3, Element.width (Element.px 150) ]
                { onChange = \nation -> SetNationToDownload nation
                , text = current
                , placeholder = Nothing -- Just (Input.placeholder [] (Element.text "nation"))
                , label = Input.labelLeft [] (Element.text "Nation to add:")
                }
    in
    Element.row [ Element.spacing 5, Element.width Element.fill, Element.alignRight ]
        [ withTip
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


nationsSelectorView : Model -> Element Msg
nationsSelectorView model =
    let
        removeButton : Iso3.NationIso3 -> Element Msg
        removeButton nation =
            Input.button
                [ Element.padding 5
                , Background.color lightGrey
                , Border.rounded 3
                , Element.alignRight
                ]
                { label = Element.text "✕"
                , onPress = Just <| RemoveNation nation
                }

        checkBox : Iso3.NationIso3 -> String -> Element Msg
        checkBox iso3Code nationName =
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
            Element.row rowStyle [ checkBox iso3Code (shortenName nationName), removeButton iso3Code ]

        rows : List (Element Msg)
        rows =
            model.graphData
                |> Dict.keys
                |> List.map (\iso3Code -> ( iso3Code, getNationName iso3Code ))
                |> List.sortBy (\( _, nationName ) -> nationName)
                |> List.map oneRow
    in
    Element.column [ Element.alignRight ] rows


plotView : Model -> Element msg
plotView model =
    Element.el [ Element.alignLeft ] (dataToPlottable model |> plotData |> Element.html)
