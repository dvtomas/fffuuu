module SwearWords exposing (severityOfMaybeSwearWordAdded)

import Czech
import Dict
import English
import String

severityOfMaybeSwearWordAdded : String -> String -> Float
severityOfMaybeSwearWordAdded oldString newString =
    let
        maybeLastWord =
            List.head << List.reverse << String.words << removeDiacritics << String.toLower

        lastWord string =
            Maybe.withDefault "" (maybeLastWord string)

        oldLastWord =
            lastWord oldString

        newLastWord =
            lastWord newString

        maybeSeverity : ( String, Float ) -> Maybe Float
        maybeSeverity ( swearWord, severity_ ) =
            let
                normalizedSwearWord =
                    (String.toLower >> removeDiacritics) swearWord
            in
                if String.contains normalizedSwearWord newLastWord && not (String.contains normalizedSwearWord oldLastWord) then
                    Just severity_

            else
                Nothing

        severities =
            List.filterMap maybeSeverity words

        severity =
            List.sum severities
    in
        if (String.length oldString < String.length newString) && (oldLastWord /= newLastWord) then
            severity
        else
            0.0


words : List ( String, Float )
words =
    List.concat [ English.words, Czech.words ]


translationList : List ( String, String )
translationList =
    [ ( "äæǽ", "ae" )
    , ( "öœ", "oe" )
    , ( "ü", "ue" )
    , ( "Ä", "Ae" )
    , ( "Ü", "Ue" )
    , ( "Ö", "Oe" )
    , ( "ÀÁÂÃÄÅĀĂĄǍΑΆẢẠẦẪẨẬẰẮẴẲẶА", "A" )
    , ( "àáâãåǻāăąǎªαάảạầấẫẩậằắẵẳặа", "a" )
    , ( "Б", "B" )
    , ( "б", "b" )
    , ( "ÇĆĈĊČ", "C" )
    , ( "çćĉċč", "c" )
    , ( "Д", "D" )
    , ( "д", "d" )
    , ( "ÐĎĐΔ", "D" )
    , ( "ðďđδ", "d" )
    , ( "ÈÉÊËĒĔĖĘĚΕΈẼẺẸỀẾỄỂỆЕЭ", "E" )
    , ( "èéêëēĕėęěέεẽẻẹềếễểệеэ", "e" )
    , ( "Ф", "F" )
    , ( "ф", "f" )
    , ( "ĜĞĠĢΓГҐ", "G" )
    , ( "ĝğġģγгґ", "g" )
    , ( "ĤĦ", "H" )
    , ( "ĥħ", "h" )
    , ( "ÌÍÎÏĨĪĬǏĮİΗΉΊΙΪỈỊИЫ", "I" )
    , ( "ìíîïĩīĭǐįıηήίιϊỉịиыї", "i" )
    , ( "Ĵ", "J" )
    , ( "ĵ", "j" )
    , ( "ĶΚК", "K" )
    , ( "ķκк", "k" )
    , ( "ĹĻĽĿŁΛЛ", "L" )
    , ( "ĺļľŀłλл", "l" )
    , ( "М", "M" )
    , ( "м", "m" )
    , ( "ÑŃŅŇΝН", "N" )
    , ( "ñńņňŉνн", "n" )
    , ( "ÒÓÔÕŌŎǑŐƠØǾΟΌΩΏỎỌỒỐỖỔỘỜỚỠỞỢО", "O" )
    , ( "òóôõōŏǒőơøǿºοόωώỏọồốỗổộờớỡởợо", "o" )
    , ( "П", "P" )
    , ( "п", "p" )
    , ( "ŔŖŘΡР", "R" )
    , ( "ŕŗřρр", "r" )
    , ( "ŚŜŞȘŠΣС", "S" )
    , ( "śŝşșšſσςс", "s" )
    , ( "ȚŢŤŦτТ", "T" )
    , ( "țţťŧт", "t" )
    , ( "ÙÚÛŨŪŬŮŰŲƯǓǕǗǙǛŨỦỤỪỨỮỬỰУ", "U" )
    , ( "ùúûũūŭůűųưǔǖǘǚǜυύϋủụừứữửựу", "u" )
    , ( "ÝŸŶΥΎΫỲỸỶỴЙ", "Y" )
    , ( "ýÿŷỳỹỷỵй", "y" )
    , ( "В", "V" )
    , ( "в", "v" )
    , ( "Ŵ", "W" )
    , ( "ŵ", "w" )
    , ( "ŹŻŽΖЗ", "Z" )
    , ( "źżžζз", "z" )
    , ( "ÆǼ", "AE" )
    , ( "ß", "ss" )
    , ( "Ĳ", "IJ" )
    , ( "ĳ", "ij" )
    , ( "Œ", "OE" )
    , ( "ƒ", "f" )
    , ( "ξ", "ks" )
    , ( "π", "p" )
    , ( "β", "v" )
    , ( "μ", "m" )
    , ( "ψ", "ps" )
    , ( "Ё", "Yo" )
    , ( "ё", "yo" )
    , ( "Є", "Ye" )
    , ( "є", "ye" )
    , ( "Ї", "Yi" )
    , ( "Ж", "Zh" )
    , ( "ж", "zh" )
    , ( "Х", "Kh" )
    , ( "х", "kh" )
    , ( "Ц", "Ts" )
    , ( "ц", "ts" )
    , ( "Ч", "Ch" )
    , ( "ч", "ch" )
    , ( "Ш", "Sh" )
    , ( "ш", "sh" )
    , ( "Щ", "Shch" )
    , ( "щ", "shch" )
    , ( "ЪъЬь", "" )
    , ( "Ю", "Yu" )
    , ( "ю", "yu" )
    , ( "Я", "Ya" )
    , ( "я", "ya" )
    ]


translationDict : Dict.Dict Char String
translationDict =
    Dict.fromList
        (List.concatMap
            (\( accentedChars, outputString ) -> List.map (\c -> ( c, outputString )) (String.toList accentedChars))
            translationList
        )


removeDiacritics : String -> String
removeDiacritics input =
    let
        translateChar : Char -> String
        translateChar c =
            Maybe.withDefault (String.fromChar c) (Dict.get c translationDict)
    in
    String.concat (List.map translateChar (String.toList input))
