module Main exposing (Model, Msg(..), init, initialModel, main, subscriptions, update, updatedModelRage, view)

import Browser
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onInput)
import Message
import RageGuy
import String
import SwearWords
import Time
import Utils



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { name : String
    , subject : String
    , message : String
    , angerFlash : Float
    , rageGuy : RageGuy.Model
    , discussion : List Message.Model
    , time : Time.Posix
    }


initialModel =
    { name = ""
    , subject = ""
    , message = ""
    , angerFlash = 0.0
    , rageGuy = RageGuy.initialModel
    , discussion = []
    , time = Time.millisToPosix 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


type Msg
    = NoOp
    | RageGuyMsg RageGuy.Msg
    | UserUpdate String
    | SubjectUpdate String
    | MessageUpdate String
    | Tick Time.Posix


updatedModelRage :
    Model
    -> Int
    -> (Model -> String)
    -> (String -> Model -> Model)
    -> String
    -> Model
updatedModelRage model maxLength stringGetter stringSetter newString =
    if String.length newString < maxLength then
        {- Max length limit -}
        let
            oldString =
                stringGetter model

            severityOfMaybeSwearWordAdded =
                SwearWords.severityOfMaybeSwearWordAdded oldString newString

            severityOfMaybeCharsAdded =
                max 0 (0.002 * toFloat (String.length newString - String.length oldString))

            addedSeverity =
                severityOfMaybeSwearWordAdded + severityOfMaybeCharsAdded

            angerFlash_ =
                15 * addedSeverity + model.angerFlash

            angerFlash__ =
                if angerFlash_ < 0.5 then
                    angerFlash_ + 0.3

                else
                    angerFlash_

            rageGuyUpdateMsg =
                RageGuy.RageUpButDontFFFUUU addedSeverity

            rageGuy =
                RageGuy.update rageGuyUpdateMsg model.rageGuy

            model_ =
                { model | rageGuy = rageGuy, angerFlash = angerFlash__ }

            model__ =
                stringSetter newString model_
        in
        model__

    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            let
                rageGuy =
                    RageGuy.update RageGuy.Tick model.rageGuy

                angerFlash =
                    Basics.max 0.0 (model.angerFlash - 0.2)

                model_ =
                    { model | rageGuy = rageGuy, angerFlash = angerFlash }

                model__ =
                    if RageGuy.shouldSendMessage model.rageGuy then
                        let
                            user =
                                Message.User model.name

                            newMessage =
                                Message.Model user time model.subject model.message

                            newDiscussion =
                                newMessage :: model.discussion
                        in
                        { model_ | discussion = newDiscussion }

                    else
                        model_
            in
            ( model__, Cmd.none )

        RageGuyMsg rageGuyMsg ->
            ( { model | rageGuy = RageGuy.update rageGuyMsg model.rageGuy }
            , Cmd.none
            )

        MessageUpdate message ->
            ( updatedModelRage model 2000 .message (\string m -> { m | message = string }) message
            , Cmd.none
            )

        SubjectUpdate subject ->
            ( updatedModelRage model 100 .subject (\string m -> { m | subject = string }) subject
            , Cmd.none
            )

        UserUpdate newUser ->
            if String.length newUser < 30 then
                ( { model | name = newUser }, Cmd.none )

            else
                ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        rageGuyView =
            Html.map RageGuyMsg (RageGuy.view model.rageGuy)

        borderedStyle =
            A.class "bordered"

        inputStyle =
            A.class "input"

        ratioTo255 ratio =
            round (Basics.max 0 (Basics.min (ratio * 255) 255))

        angerColor =
            Utils.colorToHex
                (Utils.rgb
                    (ratioTo255 model.angerFlash)
                    (ratioTo255 (model.angerFlash - 1.0))
                    (ratioTo255 (model.angerFlash - 1.0))
                )
    in
    section
        [ A.style "background" angerColor
        , A.style "color" "#FFD0D0"
        , A.style "font-size" "20px"
        , A.style "margin" "1em 1.6em"
        ]
        [ header [ borderedStyle ]
            [ h1
                [ A.class "top-label" ]
                [ text "Frustrated? Hatin' it? Roaring Rage?" ]
            ]
        , article []
            [ p [] []
            , div []
                [ input
                    [ inputStyle
                    , A.placeholder "User"
                    , A.value model.name
                    , onInput UserUpdate
                    ]
                    []
                , br [] []
                , input
                    [ inputStyle
                    , A.placeholder "Topic"
                    , A.value model.subject
                    , onInput SubjectUpdate
                    ]
                    []
                , br [] []
                , textarea
                    [ inputStyle
                    , A.style "height" "360px"
                    , A.placeholder "Enter your hatebook message and hold Derp image to post it. The more you swear the more angry he becomes!"
                    , A.value model.message
                    , onInput MessageUpdate
                    ]
                    []
                , div
                    [ A.style "width" (String.fromInt RageGuy.rageGuyImageWidth ++ "px")
                    , A.style "margin" "0 auto"
                    ]
                    [ rageGuyView ]
                , hr [] []
                ]
            , article []
                [ div [] (List.intersperse (hr [] []) <| List.map Message.view model.discussion)
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick
