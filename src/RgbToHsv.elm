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
    , rPrime = prime input.r
    , gPrime = prime input.g
    , bPrime = prime input.b
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


prime : Int -> Float
prime myColor =
    toFloat myColor / 255


cMax : Data -> Data
cMax data =
    { data | cMax = max data.rPrime <| max data.gPrime data.bPrime }


cMin : Data -> Data
cMin data =
    { data | cMin = min data.rPrime <| min data.gPrime data.bPrime }


delta : Data -> Data
delta data =
    { data | delta = data.cMax - data.cMin }


solveH : Data -> Data
solveH data =
    if delta data == cMin data then
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
    if data.cMax == data.bPrime then
        { data | s = 0 }

    else
        { data | s = (data.delta / data.cMax) * 100 }


solveV : Data -> Data
solveV data =
    { data | v = data.cMax * 100 }


floatMod a b =
    if a >= b then
        floatMod (a - b) b

    else
        a
