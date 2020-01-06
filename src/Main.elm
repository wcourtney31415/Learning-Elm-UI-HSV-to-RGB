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
    { h = 173
    , s = 0.5
    , v = 0.2
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
            { model
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
            , Input.button
                [ Background.color buttonColor
                , Font.color black
                , paddingXY 10 10
                , Border.rounded 3
                , centerX
                ]
                { onPress = Just <| ConvertBtnPressed model
                , label = Element.text "Convert"
                }
            ]


buttonColor =
    rgb255 230 230 230


black =
    rgb255 0 1 0
