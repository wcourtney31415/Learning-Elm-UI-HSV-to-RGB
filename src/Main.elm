module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { h : Float
    , s : Float
    , v : Float
    , r : Float
    , g : Float
    , b : Float
    , c : Float
    , x : Float
    , m : Float
    }


init : Model
init =
    { h = 173
    , s = 0.5
    , v = 0.2
    , r = 0
    , g = 0
    , b = 0
    , c = 0
    , x = 0
    , m = 0
    }


hsvToRgb : Model -> Model
hsvToRgb model =
    unprimeRGB <| calcRange <| calcM <| calcX <| calcC model


calcC : Model -> Model
calcC model =
    { model | c = model.v * model.s }


calcM : Model -> Model
calcM model =
    { model | m = model.v - model.c }


calcX : Model -> Model
calcX model =
    { model | x = model.c * (1 - abs (floatModulous (model.h / 60) 2 0 - 1)) }


floatModulous : Float -> Float -> Int -> Float
floatModulous destination divisor iterator =
    if ((divisor * toFloat iterator) + 1) > destination then
        destination - (divisor * toFloat iterator)

    else
        floatModulous destination divisor (iterator + 1)


calcRange : Model -> Model
calcRange model =
    if 0 <= model.h && model.h < 60 then
        { model | r = model.c, g = model.x, b = 0 }

    else if 60 <= model.h && model.h < 120 then
        { model | r = model.x, g = model.c, b = 0 }

    else if 120 <= model.h && model.h < 180 then
        { model | r = 0, g = model.c, b = model.x }

    else if 180 <= model.h && model.h < 240 then
        { model | r = 0, g = model.x, b = model.c }

    else if 240 <= model.h && model.h < 300 then
        { model | r = model.x, g = 0, b = model.c }

    else if 300 <= model.h && model.h < 360 then
        { model | r = model.c, g = 0, b = model.x }

    else
        model


unprime : Float -> Float -> Float
unprime myColor m =
    (myColor + m) * 255


unprimeRGB : Model -> Model
unprimeRGB model =
    let
        myR =
            unprime model.r model.m

        myG =
            unprime model.g model.m

        myB =
            unprime model.b model.m
    in
    { model | r = myR, g = myG, b = myB }


type Msg
    = HueChange
    | SaturationChange
    | ValueChange


update : Msg -> Model -> Model
update msg model =
    hsvToRgb model


view : Model -> Html.Html Msg
view model =
    let
        r =
            String.fromFloat model.r

        g =
            String.fromFloat model.g

        b =
            String.fromFloat model.b
    in
    Element.layout
        []
    <|
        Element.column []
            [ Element.el [] (text ("R: " ++ r ++ " G: " ++ g ++ " B: " ++ b))
            ]
