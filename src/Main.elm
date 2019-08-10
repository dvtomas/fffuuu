module Main exposing (Model, Msg(..), init, initialModel, main, subscriptions, update, updatedModelRage, view)

import Browser
import Discussion
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onInput)
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
    , topic : String
    , message : String
    , angerFlash : Float
    , rageGuy : RageGuy.Model
    , discussion : List Discussion.Message
    , time : Time.Posix
    }


initialModel =
    { name = ""
    , topic = ""
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
    | ClickRageGuy
    | UserUpdate String
    | TopicUpdate String
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

            angerFlash =
                (50 * addedSeverity + model.angerFlash) |> Basics.min 5.0

            rageGuy =
                RageGuy.update (RageGuy.RageUp addedSeverity) model.rageGuy

            model_ =
                { model | rageGuy = rageGuy, angerFlash = angerFlash }

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

                angerFlash = model.angerFlash - 0.2
            in
            ( { model | time = time, rageGuy = rageGuy, angerFlash = angerFlash }, Cmd.none )

        RageGuyMsg rageGuyMsg ->
            let
                oldAnger =
                    model.rageGuy.targetAnger

                newRageGuy =
                    RageGuy.update rageGuyMsg model.rageGuy

                angerDiff =
                    newRageGuy.targetAnger - oldAnger
            in
            ( { model | rageGuy = newRageGuy, angerFlash = (model.angerFlash + angerDiff) * 10.0 }
            , Cmd.none
            )

        ClickRageGuy ->
            let
                rageGuy =
                    model.rageGuy

                newModel =
                    if rageGuy.targetAnger >= 1.0 && not rageGuy.isRaging && not (String.isEmpty model.message) then
                        -- Start raging, post the message
                        let
                            user =
                                Discussion.User model.name

                            newMessage =
                                Discussion.Message user model.time model.topic model.message

                            newDiscussion =
                                newMessage :: model.discussion
                        in
                        { model
                            | topic = ""
                            , message = ""
                            , discussion = newDiscussion
                            , rageGuy = { rageGuy | isRaging = True }
                        }

                    else if rageGuy.isRaging then
                        -- Cool down
                        { model | angerFlash = 5.0, rageGuy = { rageGuy | targetAnger = 0.0, isRaging = False, mood = RageGuy.Neutral } }

                    else
                        -- Increment anger and flash
                        { model
                            | angerFlash = model.angerFlash + 1.0
                            , rageGuy = RageGuy.update (RageGuy.RageUp 0.05) model.rageGuy
                        }
            in
            ( newModel, Cmd.none )

        MessageUpdate message ->
            ( updatedModelRage model 2000 .message (\string m -> { m | message = string }) message
            , Cmd.none
            )

        TopicUpdate topic ->
            ( updatedModelRage model 100 .topic (\string m -> { m | topic = string }) topic
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
        angerColor =
            let
                interpolate start end ratio =
                    round (toFloat start + ratio * toFloat (end - start))

                interpolate3 a b c d p =
                    if p < 0.0 then
                        a

                    else if p < 1.0 then
                        interpolate a b p

                    else if p < 2.0 then
                        interpolate b c (p - 1.0)

                    else if p < 3.0 then
                        interpolate c d (p - 2.0)

                    else
                        d

                now =
                    toFloat (Time.posixToMillis model.time)

                anger =
                    if model.rageGuy.targetAnger < 1.0 then
                        model.angerFlash * 0.1

                    else if not model.rageGuy.isRaging then
                        -- Click to rage
                        (1.5 + sin (now / 500.0))
                            + (0.5 * sin (0.3 + now / 400.0))
                            + (0.4 * cos (0.8 + now / 300.0))

                    else
                        (1.0 + sin (now / 500.0))
                            + (0.5 * sin (0.3 + now / 100.0))
                            + (0.4 * cos (0.8 + now / 200.0))

                angerColorComponent a b c d =
                    interpolate3 a b c d anger
            in
            Utils.colorToHex
                (Utils.rgb
                    (angerColorComponent 255 255 192 64)
                    (angerColorComponent 255 128 64 32)
                    (angerColorComponent 255 128 64 32)
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

        topicInput =
            input
                [ A.type_ "Text"
                , A.placeholder "Topic"
                , A.value model.topic
                , onInput TopicUpdate
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
            div [ A.class "message" ]
                [ b [] [ text message.user.username ]
                , text " "
                , i [] [ text (formatTime message.timestamp) ]
                , text " "
                , b [] [ text message.topic ]
                , p [] [ text message.body ]
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
                    , topicInput
                    , br [] []
                    , messageInput
                    ]
                , div [ A.class "column", Html.Events.onClick ClickRageGuy ] [ rageGuyView ]
                ]
            , hr [] []
            , div [] (List.intersperse (hr [] []) <| List.map viewMessage model.discussion)
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick
