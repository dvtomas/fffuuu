module Main exposing (Model, Msg(..), init, initialModel, main, subscriptions, update, updatedModelRage, view)

import Browser
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onInput)
import Discussion
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
    , discussion : List Discussion.Message
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
                                Discussion.User model.name

                            newMessage =
                                Discussion.Message user time model.subject model.message

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
        ratioTo255 ratio =
            round (Basics.max 0 (Basics.min (ratio * 255) 255))

        angerColor =
            Utils.colorToHex
                (Utils.rgb
                    255
                    (ratioTo255 (0.98 - model.angerFlash))
                    (ratioTo255 (0.97 - model.angerFlash))
                )

        fffuuuHeader =
            header [ A.class "bordered" ]
                [ h1
                    [ A.class "top-label" ]
                    [ text "Frustrated? Hatin' it? Roaring Rage?", hr [] [] ]
                ]

        userNameInput =
            input
                [ A.type_ "Text"
                , A.placeholder "Your  name"
                , A.value model.name
                , onInput UserUpdate
                ]
                []

        subjectInput =
            input
                [ A.type_ "Text"
                , A.placeholder "Topic"
                , A.value model.subject
                , onInput SubjectUpdate
                ]
                []

        messageInput =
            textarea
                [ A.style "height" "200"
                , A.placeholder "Enter your hate message. The more you swear the angrier Rage guy becomes! Click him to bully him even more, make him rage to have your message posted!"
                , A.value model.message
                , onInput MessageUpdate
                ]
                []

        rageGuyView =
            div
                [ A.style "width" (String.fromInt RageGuy.rageGuyImageWidth ++ "px")
                , A.class "derp"
                ]
                [ Html.map RageGuyMsg (RageGuy.view model.rageGuy) ]

        formatTime : Time.Posix -> String
        formatTime time =
            String.fromInt (Time.toHour Time.utc time)
                ++ ":"
                ++ String.fromInt (Time.toMinute Time.utc time)
                ++ ":"
                ++ String.fromInt (Time.toSecond Time.utc time)
                ++ " (UTC)"

        viewMessage message =
          div [A.class "message"] [
            b [] [text (message.user.username)],
            text " ",
            i [] [text (formatTime message.timestamp)],
            text " ",
            b [] [text message.subject],
            p [] [text message.body]
          ]

        container html =
            div [ A.class "container" ] html

        row html =
            div [ A.class "row" ] html

        col html =
            div [ A.class "column" ] html
    in
    section
        [ A.style "background" angerColor
        , A.style "color" "#FFD0D0"
        , A.style "font-size" "20px"
        ]
        [ fffuuuHeader
        , container
            [ row
                [ col
                    [ userNameInput
                    , br [] []
                    , subjectInput
                    , br [] []
                    , messageInput
                    ]
                , col [ rageGuyView ]
                ]
            , hr [] []
            , div [] (List.intersperse (hr [] []) <| List.map viewMessage model.discussion)
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick
