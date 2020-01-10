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
    , r : Int
    , g : Int
    , b : Int
    , rPrime : Float
    , gPrime : Float
    , bPrime : Float
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
    , rPrime = 0
    , gPrime = 0
    , bPrime = 0
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
        { data | rPrime = data.c, gPrime = data.x, bPrime = 0 }

    else if 60 <= data.h && data.h < 120 then
        { data | rPrime = data.x, gPrime = data.c, bPrime = 0 }

    else if 120 <= data.h && data.h < 180 then
        { data | rPrime = 0, gPrime = data.c, bPrime = data.x }

    else if 180 <= data.h && data.h < 240 then
        { data | rPrime = 0, gPrime = data.x, bPrime = data.c }

    else if 240 <= data.h && data.h < 300 then
        { data | rPrime = data.x, gPrime = 0, bPrime = data.c }

    else if 300 <= data.h && data.h < 360 then
        { data | rPrime = data.c, gPrime = 0, bPrime = data.x }

    else
        data


unprime : Float -> Float -> Float
unprime myColor m =
    (myColor + m) * 255


solveRGB : Data -> Results
solveRGB data =
    let
        myR =
            round <| unprime data.rPrime data.m

        myG =
            round <| unprime data.gPrime data.m

        myB =
            round <| unprime data.bPrime data.m
    in
    { r = myR, g = myG, b = myB }
