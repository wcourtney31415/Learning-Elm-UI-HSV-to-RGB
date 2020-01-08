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
    { h = 0
    , s = 0
    , v = 0
    , r = 0
    , g = 0
    , b = 0
    }


type Msg
    = SliderMoved Model


update : Msg -> Model -> Model
update msg model =
    case msg of
        SliderMoved myModel ->
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
        Element.column
            [ width fill
            , spacing 20
            , padding 40
            ]
            [ header
            , hsvLabel model
            , colorBlock model
            , rgbLabel model
            , Element.row
                [ width fill
                , padding 20
                , spacing 20
                ]
                [ hsvSlider model
                , rgbSlider model
                ]
            ]


colorBlock model =
    Input.button
        [ Background.color <| rgb255 model.r model.g model.b
        , width <| px 200
        , height <| px 200
        , centerX
        ]
        { onPress = Nothing
        , label = text " "
        }


header =
    Element.el
        [ centerX ]
        (text
            "HSV <--> RGB"
        )


hsvLabel model =
    Element.el
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


rgbLabel model =
    Element.el
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


hsvSlider : Model -> Element Msg
hsvSlider model =
    Element.column [ width fill ]
        [ Input.slider
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
            { onChange = \new -> SliderMoved { model | h = round new }
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
            { onChange = \new -> SliderMoved { model | s = new }
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
            { onChange = \new -> SliderMoved { model | v = new }
            , label = Input.labelAbove [] (text "Value")
            , min = 0
            , max = 1
            , step = Just 0.01
            , value = model.v
            , thumb =
                Input.defaultThumb
            }
        ]


rgbSlider : Model -> Element Msg
rgbSlider model =
    Element.column [ width fill ]
        [ Input.slider
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
            { onChange = \new -> SliderMoved { model | h = round new }
            , label = Input.labelAbove [] (text "Red")
            , min = 0
            , max = 255
            , step = Just 1
            , value = toFloat model.r
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
            { onChange = \new -> SliderMoved { model | s = new }
            , label = Input.labelAbove [] (text "Green")
            , min = 0
            , max = 255
            , step = Just 1
            , value = toFloat model.g
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
            { onChange = \new -> SliderMoved { model | v = new }
            , label = Input.labelAbove [] (text "Blue")
            , min = 0
            , max = 255
            , step = Just 1
            , value = toFloat model.b
            , thumb =
                Input.defaultThumb
            }
        ]


buttonColor =
    rgb255 230 230 230


black =
    rgb255 0 1 0
