module RageGuy exposing
    ( Model
    , Mood(..)
    , Msg(..)
    , angerToMood
    , initialModel
    , isMoodBefore
    , maybeMoodTransitionFrameAfter
    , nextMood
    , rageGuyImageWidth
    , relativeFrameForStaticMood
    , shouldSendMessage
    , update
    , view
    )

import Array
import Dict
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onClick)


type alias Anger =
    Float


type Mood
    = Neutral
    | Annoyed
    | Pissed
    | Drooling
    | VergeOfExplosion
    | FFFUUU


shouldSendMessage model =
    model.mood == FFFUUU && model.clock == 1


isMoodBefore : Mood -> Mood -> Bool
isMoodBefore mood1 mood2 =
    if mood1 == FFFUUU then
        False

    else if nextMood mood1 == mood2 then
        True

    else
        isMoodBefore (nextMood mood1) mood2


nextMood : Mood -> Mood
nextMood mood =
    case mood of
        Neutral ->
            Annoyed

        Annoyed ->
            Pissed

        Pissed ->
            Drooling

        Drooling ->
            VergeOfExplosion

        VergeOfExplosion ->
            FFFUUU

        FFFUUU ->
            FFFUUU


type alias Model =
    { clock : Int
    , mood : Mood
    , isRaging : Bool
    , isMoodInTransition : Bool
    , lastRelativeFrame : Int
    , targetAnger : Anger
    }


angerToMood : Anger -> Bool -> Mood
angerToMood anger isRaging =
    if isRaging then
        FFFUUU

    else if anger < 0.1 then
        Neutral

    else if anger < 0.25 then
        Annoyed

    else if anger < 0.5 then
        Pissed

    else if anger < 0.8 then
        Drooling

    else
        VergeOfExplosion


initialModel : Model
initialModel =
    { clock = 0
    , mood = Neutral
    , isRaging = False
    , isMoodInTransition = False
    , lastRelativeFrame = 0
    , targetAnger = 0
    }


type alias FramesDefinition =
    { staticFrames : List Int
    , transitions : Dict.Dict Int Int
    }


framesDefinition : Mood -> FramesDefinition
framesDefinition mood =
    let
        repeat count list =
            List.concat (List.repeat count list)

        reverse =
            List.reverse

        mapRangeTo list target =
            List.map (\x -> ( x, target )) list

        sequence list =
            List.map (\x -> ( x, x + 1 )) list

        rangeAndSequence staticCount transitionCount =
            List.concat
                [ mapRangeTo (List.range 0 (staticCount - 1)) staticCount
                , sequence (List.range staticCount (transitionCount + staticCount))
                ]
    in
    case mood of
        Neutral ->
            let
                down =
                    [ 0, 1, 2 ]

                downBlink =
                    [ 3, 4 ]

                downToUp =
                    [ 5, 6, 7 ]

                up =
                    [ 8, 9, 10 ]

                upBlink =
                    [ 11, 12 ]

                staticFrames =
                    List.concat
                        [ repeat 5 down
                        , downBlink
                        , repeat 7 down
                        , downToUp
                        , repeat 3 up
                        , repeat 12 up
                        , upBlink
                        , repeat 3 up
                        , reverse downToUp
                        ]

                transitions =
                    List.concat
                        [ mapRangeTo (List.range 0 4) 5
                        , sequence (List.range 5 7)
                        , mapRangeTo (List.range 8 12) 13
                        , [ ( 13, 14 ) ]
                        ]
            in
            { staticFrames = staticFrames, transitions = Dict.fromList transitions }

        Annoyed ->
            { staticFrames = [ 0, 1, 2 ], transitions = Dict.fromList (rangeAndSequence 3 6) }

        Pissed ->
            let
                down =
                    [ 0, 1, 2 ]

                downToUp =
                    [ 3, 4, 5 ]

                upToDown =
                    reverse downToUp

                up =
                    [ 6, 7, 8 ]

                staticFrames =
                    List.concat
                        [ repeat 12 down
                        , downToUp
                        , repeat 5 up
                        , upToDown
                        , downToUp
                        , repeat 8 up
                        , upToDown
                        ]

                transitions =
                    List.concat
                        [ mapRangeTo (List.range 0 2) 3
                        , sequence (List.range 3 5)
                        , mapRangeTo (List.range 6 8) 9
                        , sequence (List.range 9 10)
                        ]
            in
            { staticFrames = staticFrames, transitions = Dict.fromList transitions }

        Drooling ->
            let
                openEyes =
                    [ 0, 1, 2, 3, 4 ]

                openToClose =
                    [ 5 ]

                closesToOpen =
                    reverse openToClose

                closedEyes =
                    [ 6, 7, 8, 9, 10 ]

                staticFrames =
                    List.concat
                        [ repeat 12 openEyes
                        , openToClose
                        , repeat 3 closedEyes
                        , closesToOpen
                        , repeat 7 openEyes
                        , openToClose
                        , repeat 8 closedEyes
                        , closesToOpen
                        , repeat 3 openEyes
                        , openToClose
                        , repeat 1 closedEyes
                        , closesToOpen
                        ]

                transitions =
                    List.concat
                        [ mapRangeTo (List.range 0 10) 11
                        , sequence (List.range 11 13)
                        ]
            in
            { staticFrames = staticFrames, transitions = Dict.fromList transitions }

        VergeOfExplosion ->
            let
                openEyes =
                    [ 0, 1, 2, 3 ]

                openToClose =
                    [ 4 ]

                closesToOpen =
                    reverse openToClose

                closedEyes =
                    [ 5, 6 ]

                staticFrames =
                    List.concat
                        [ repeat 6 openEyes
                        , openToClose
                        , repeat 2 closedEyes
                        , closesToOpen
                        , repeat 5 openEyes
                        , openToClose
                        , repeat 8 closedEyes
                        , closesToOpen
                        , repeat 3 openEyes
                        , openToClose
                        , repeat 1 closedEyes
                        , closesToOpen
                        ]

                transitions =
                    List.concat
                        [ mapRangeTo (List.range 0 4) 5
                        , sequence (List.range 5 8)
                        ]
            in
            { staticFrames = staticFrames, transitions = Dict.fromList transitions }

        FFFUUU ->
            { staticFrames = [ 0, 1, 2 ], transitions = Dict.empty }


relativeFrameForStaticMood : Int -> Mood -> Int
relativeFrameForStaticMood clock mood =
    let
        cycle =
            Array.fromList (framesDefinition mood).staticFrames
    in
    Maybe.withDefault 0 (Array.get (modBy (Array.length cycle) clock) cycle)


maybeMoodTransitionFrameAfter : Int -> Mood -> Maybe Int
maybeMoodTransitionFrameAfter lastFrame mood =
    Dict.get lastFrame (framesDefinition mood).transitions


type Msg
    = Tick
    | RageUp Float


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            let
                targetMood =
                    angerToMood model.targetAnger model.isRaging

                newModel =
                    if model.isMoodInTransition then
                        if isMoodBefore model.mood targetMood then
                            case maybeMoodTransitionFrameAfter model.lastRelativeFrame model.mood of
                                Just nextFrame ->
                                    { model | lastRelativeFrame = nextFrame }

                                Nothing ->
                                    { model
                                        | clock = 0
                                        , lastRelativeFrame = 0
                                        , mood = nextMood model.mood
                                        , isMoodInTransition = False
                                    }

                        else
                            -- Should not happen
                            model

                    else if model.mood == targetMood then
                        { model
                            | lastRelativeFrame = relativeFrameForStaticMood model.clock model.mood
                            , clock = model.clock + 1
                        }

                    else
                        { model | isMoodInTransition = True }
            in
            newModel

        RageUp amount ->
            { model | targetAnger = model.targetAnger + amount }


rageGuyImageWidth : number
rageGuyImageWidth =
    540


startFrame : Mood -> Int
startFrame mood =
    case mood of
        Neutral ->
            0

        Annoyed ->
            15

        Pissed ->
            24

        Drooling ->
            36

        VergeOfExplosion ->
            51

        FFFUUU ->
            62


view model =
    let
        height =
            360

        index =
            startFrame model.mood + model.lastRelativeFrame

        leftSpritePosition =
            String.fromInt (-rageGuyImageWidth * index) ++ "px"

        numberOfFFFRows =
            5

        fffuuuuTextCut limit =
            List.map
                (\rowIndex ->
                    let
                        rowLength =
                            7

                        {- Ugly, but I'm too sleeepy to do it better -}
                        rowTemplate =
                            if rowIndex < numberOfFFFRows then
                                [ "F", "F", "F", "F", "F", "F", "F" ]

                            else if rowIndex == numberOfFFFRows then
                                [ "F", "F", "F", "U", "U", "U", "U" ]

                            else
                                [ "U", "U", "U", "U", "U", "U", "U" ]

                        chars =
                            Basics.max 0 (limit - (rowIndex * rowLength))
                    in
                    List.take chars rowTemplate
                )
                (List.range 0 (numberOfFFFRows * 2))

        randomShifts =
            Array.fromList [ -1, 2, 3, -2, 0, 2, -3, 1, 2, 1, 2, 1, 0, -1, -2, 2, 1, 0, 1, -3, 1, -2 ]

        randomShift n =
            Maybe.withDefault 0 (Array.get (modBy (Array.length randomShifts) (n + model.clock)) randomShifts)

        letterDiv xIndex yIndex letter =
            let
                xPosition =
                    xIndex * 20 + 390 + randomShift ((xIndex * 3 + (yIndex * 17)) * 35)

                yPosition =
                    yIndex * 30 + 15 + randomShift ((xIndex * 7) + (yIndex * 5))
            in
            div
                [ A.style "z-index" "100"
                , A.style "position" "absolute"
                , A.style "left" (String.fromInt xPosition ++ "px")
                , A.style "top" (String.fromInt yPosition ++ "px")
                , A.style "color" "#FF0000"
                , A.style "font-weight" "bold"
                , A.style "font-size" "30px"
                ]
                [ text letter ]

        rowDivs yIndex letters =
            List.indexedMap (\xIndex letter -> letterDiv xIndex yIndex letter) letters

        textDivs texts =
            List.concat (List.indexedMap (\yIndex letters -> rowDivs yIndex letters) texts)

        fffuuuTextDivs =
            if model.mood == FFFUUU then
                textDivs (fffuuuuTextCut (model.clock * 3))

            else
                []
    in
    div
        [ A.style "position" "relative"
        , A.width rageGuyImageWidth
        , A.style "width" (String.fromInt rageGuyImageWidth ++ "px")
        , A.style "height" (String.fromInt height ++ "px")
        , A.style "background" ("url('img/RageGuySprites.jpg') " ++ leftSpritePosition ++ " 0px no-repeat")
        ]
        fffuuuTextDivs
