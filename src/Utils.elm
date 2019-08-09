module Utils exposing(colorToHex, rgb)
import String
import Char

type alias Color = {red: Int, green: Int, blue: Int, alpha: Int}

rgb red green blue = {red = red, green = green, blue = blue, alpha = 255}

--Converts a color to a hexadecimal string. hexToColor (rgb 255 0 0) -- "#ff0000"
colorToHex : Color -> String
colorToHex cl =
    let
        { red, green, blue, alpha } = cl
    in
        "#" ++ (toHex red) ++ (toHex green) ++ (toHex blue)


toHex : Int -> String
toHex n =
    let
        hex = toRadix n
    in
        if String.length hex == 1 then
            "0" ++ hex
        else
            hex


toRadix : Int -> String
toRadix n =
    let
        getChr c =
            if c < 10 then
                String.fromInt c
            else
                String.fromChar <| Char.fromCode (87 + c)
    in
        if n < 16 then
            getChr n
        else
            (toRadix (n // 16)) ++ (getChr (modBy 16 n))
