module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import HsvToRgb exposing (hsvToRgb)
import Html exposing (Html)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { h : Int
    , s : Float
    , v : Float
    , r : Int
    , g : Int
    , b : Int
    }


init : Model
init =
    { h = 120
    , s = 1
    , v = 0.5
    , r = 0
    , g = 0
    , b = 0
    }


type Msg
    = ConvertBtnPressed Model


update : Msg -> Model -> Model
update msg model =
    case msg of
        ConvertBtnPressed myModel ->
            let
                input =
                    { h = myModel.h
                    , s = myModel.s
                    , v = myModel.v
                    }

                results =
                    hsvToRgb input
            in
            { myModel
                | r = results.r
                , g = results.g
                , b = results.b
            }


view model =
    Element.layout
        []
    <|
        Element.column [ width fill, spacing 20, padding 40 ]
            [ Element.el
                [ centerX ]
                (text
                    ("H: "
                        ++ String.fromInt model.h
                        ++ " S: "
                        ++ String.fromFloat model.s
                        ++ " V: "
                        ++ String.fromFloat model.v
                    )
                )
            , Element.el
                [ centerX ]
                (text
                    ("R: "
                        ++ String.fromInt model.r
                        ++ " G: "
                        ++ String.fromInt model.g
                        ++ " B: "
                        ++ String.fromInt model.b
                    )
                )
            , Input.slider
                [ Element.height (Element.px 30)
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color buttonColor
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = \new -> ConvertBtnPressed { model | h = round new }
                , label = Input.labelAbove [] (text "Hue")
                , min = 0
                , max = 360
                , step = Just 1
                , value = toFloat model.h
                , thumb =
                    Input.defaultThumb
                }
            , Input.slider
                [ Element.height (Element.px 30)
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color buttonColor
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = \new -> ConvertBtnPressed { model | s = new }
                , label = Input.labelAbove [] (text "Saturation")
                , min = 0
                , max = 1
                , step = Just 0.01
                , value = model.s
                , thumb =
                    Input.defaultThumb
                }
            , Input.slider
                [ Element.height (Element.px 30)
                , Element.behindContent
                    (Element.el
                        [ Element.width Element.fill
                        , Element.height (Element.px 2)
                        , Element.centerY
                        , Background.color buttonColor
                        , Border.rounded 2
                        ]
                        Element.none
                    )
                ]
                { onChange = \new -> ConvertBtnPressed { model | v = new }
                , label = Input.labelAbove [] (text "Value")
                , min = 0
                , max = 1
                , step = Just 0.01
                , value = model.v
                , thumb =
                    Input.defaultThumb
                }
            ]


buttonColor =
    rgb255 230 230 230


black =
    rgb255 0 1 0
