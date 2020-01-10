module RgbToHsv exposing (rgbToHsv)

import Round


type alias Input =
    { r : Int
    , g : Int
    , b : Int
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
    , cMax : Float
    , cMin : Float
    , delta : Float
    }


type alias Results =
    { h : Int
    , s : Float
    , v : Float
    }


dataIncluding : Input -> Data
dataIncluding input =
    { h = 303
    , s = 303
    , v = 303
    , r = input.r
    , g = input.g
    , b = input.b
    , rPrime = 0
    , gPrime = 0
    , bPrime = 0
    , cMax = 0
    , cMin = 0
    , delta = 0
    }


primeRGB : Data -> Data
primeRGB data =
    { data | rPrime = prime data.r, gPrime = prime data.g, bPrime = prime data.b }


solveHSV : Data -> Results
solveHSV data =
    let
        myH =
            data.h

        myS =
            Round.roundNum 2 data.s

        myV =
            Round.roundNum 2 data.v
    in
    { h = myH, s = myS, v = myV }


rgbToHsv : Input -> Results
rgbToHsv input =
    dataIncluding input |> primeRGB |> solveCMin |> solveCMax |> solveDelta |> solveH |> solveS |> solveV |> solveHSV


prime : Int -> Float
prime myColor =
    toFloat myColor / 255


solveCMax : Data -> Data
solveCMax data =
    { data | cMax = max data.rPrime <| max data.gPrime data.bPrime }


solveCMin : Data -> Data
solveCMin data =
    { data | cMin = min data.rPrime <| min data.gPrime data.bPrime }


solveDelta : Data -> Data
solveDelta data =
    { data | delta = data.cMax - data.cMin }


solveH : Data -> Data
solveH data =
    if data.delta == data.cMin then
        { data | h = 0 }

    else if data.cMax == data.rPrime then
        { data | h = round (floatMod (60 * ((data.gPrime - data.bPrime) / data.delta) + 360) 360) }

    else if data.cMax == data.gPrime then
        { data | h = round (floatMod (60 * ((data.bPrime - data.rPrime) / data.delta) + 120) 360) }

    else if data.cMax == data.bPrime then
        { data | h = round (floatMod (60 * ((data.rPrime - data.gPrime / data.delta) + 240)) 360) }

    else
        data


solveS : Data -> Data
solveS data =
    if data.cMax == 0 then
        { data | s = 0 }

    else
        { data | s = data.delta / data.cMax }


solveV : Data -> Data
solveV data =
    { data | v = data.cMax }


floatMod a b =
    if a >= b then
        floatMod (a - b) b

    else
        a
