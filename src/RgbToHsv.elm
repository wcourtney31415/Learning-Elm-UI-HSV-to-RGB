module RgbToHsv exposing (rgbToHsv)


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
    , cMax : Int
    , cMin : Int
    , delta : Int
    }


type alias Results =
    { h : Int
    , s : Float
    , v : Float
    }


dataIncluding : Input -> Data
dataIncluding input =
    { h = 0
    , s = 0
    , v = 0
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


solveHSV : Data -> Results
solveHSV data =
    let
        myH =
            data.h

        myS =
            data.s

        myV =
            data.v
    in
    { h = myH, s = myS, v = myV }


rgbToHsv : Input -> Results
rgbToHsv input =
    solveHSV <| solveV <| solveS <| solveH <| dataIncluding input


prime : Float -> Float
prime myColor =
    myColor / 255


cMax : Data -> Data
cMax data =
    { data | cMax = max data.r <| max data.g data.b }


cMin : Data -> Data
cMin data =
    { data | cMin = min data.r <| min data.g data.b }


delta : Data -> Data
delta data =
    { data | delta = data.cMax - data.cMin }


solveH : Data -> Data
solveH data =
    if delta data == cMin data then
        { data | h = 0 }

    else if data.cMax == data.r then
        { data | h = round (floatMod (60 * ((toFloat data.g - toFloat data.b) / toFloat data.delta) + 360) 360) }

    else if data.cMax == data.g then
        { data | h = round (floatMod (60 * ((toFloat data.b - toFloat data.r) / toFloat data.delta) + 120) 360) }

    else if data.cMax == data.b then
        { data | h = round (floatMod (60 * ((toFloat data.r - toFloat data.g / toFloat data.delta) + 240)) 360) }

    else
        data


solveS : Data -> Data
solveS data =
    if data.cMax == data.b then
        { data | s = 0 }

    else
        { data | s = (toFloat data.delta / toFloat data.cMax) * 100 }


solveV : Data -> Data
solveV data =
    { data | v = toFloat data.cMax * 100 }


floatMod a b =
    if a >= b then
        floatMod (a - b) b

    else
        a
