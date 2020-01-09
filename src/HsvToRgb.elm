module HsvToRgb exposing (hsvToRgb)


type alias Input =
    { h : Int
    , s : Float
    , v : Float
    }


type alias Data =
    { h : Int
    , s : Float
    , v : Float
    , r : Float
    , g : Float
    , b : Float
    , c : Float
    , x : Float
    , m : Float
    }


type alias Results =
    { r : Int
    , g : Int
    , b : Int
    }


dataIncluding : Input -> Data
dataIncluding input =
    { h = input.h
    , s = input.s
    , v = input.v
    , r = 0
    , g = 0
    , b = 0
    , c = 0
    , x = 0
    , m = 0
    }


hsvToRgb : Input -> Results
hsvToRgb input =
    solveRGB <| calcRange <| solveM <| solveX <| solveC <| dataIncluding input


solveC : Data -> Data
solveC data =
    { data | c = data.v * data.s }


solveM : Data -> Data
solveM data =
    { data | m = data.v - data.c }


solveX : Data -> Data
solveX data =
    { data | x = data.c * (1 - abs (floatMod (toFloat data.h / 60) 2 - 1)) }


floatMod a b =
    if a >= b then
        floatMod (a - b) b

    else
        a


calcRange : Data -> Data
calcRange data =
    if 0 <= data.h && data.h < 60 then
        { data | r = data.c, g = data.x, b = 0 }

    else if 60 <= data.h && data.h < 120 then
        { data | r = data.x, g = data.c, b = 0 }

    else if 120 <= data.h && data.h < 180 then
        { data | r = 0, g = data.c, b = data.x }

    else if 180 <= data.h && data.h < 240 then
        { data | r = 0, g = data.x, b = data.c }

    else if 240 <= data.h && data.h < 300 then
        { data | r = data.x, g = 0, b = data.c }

    else if 300 <= data.h && data.h < 360 then
        { data | r = data.c, g = 0, b = data.x }

    else
        data


unprime : Float -> Float -> Float
unprime myColor m =
    (myColor + m) * 255


solveRGB : Data -> Results
solveRGB data =
    let
        myR =
            round <| unprime data.r data.m

        myG =
            round <| unprime data.g data.m

        myB =
            round <| unprime data.b data.m
    in
    { r = myR, g = myG, b = myB }
