module Main exposing (Model, Msg(..), init, initialModel, main, subscriptions, update, updatedModelRage, view)

import Browser
import Discussion
import Html exposing (..)
import Html.Attributes as A
import Html.Events exposing (onInput)
import RageGuy
import String
import SwearWords
import Task
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
    { username : String
    , topic : String
    , message : String
    , angerFlash : Float
    , rageGuy : RageGuy.Model
    , discussion : List Discussion.Message
    , zone : Time.Zone
    , time : Time.Posix
    , nextFetchMessages : Time.Posix
    }


initialModel =
    { username = ""
    , topic = ""
    , message = ""
    , angerFlash = 0.0
    , rageGuy = RageGuy.initialModel
    , discussion = []
    , time = Time.millisToPosix 0
    , zone = Time.utc
    , nextFetchMessages = Time.millisToPosix 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Task.perform AdjustTimeZone Time.here )


type Msg
    = NoOp
    | RageGuyMsg RageGuy.Msg
    | DiscussionMsg Discussion.Msg
    | ClickRageGuy
    | UserUpdate String
    | TopicUpdate String
    | MessageUpdate String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone


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

            charsAdded =
                toFloat (String.length newString - String.length oldString)

            severityOfMaybeCharsAdded =
                max 0 (0.002 * charsAdded)

            addedSeverity =
                severityOfMaybeSwearWordAdded + severityOfMaybeCharsAdded

            angerFlash =
                (5 * addedSeverity + model.angerFlash) + 0.15 * charsAdded

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


restDbError description httpError time =
    { username = "RestDB communication"
    , timestamp = time
    , topic = description
    , message = Discussion.httpErrorToString httpError
    }


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
                    model.angerFlash - 0.13 |> Basics.max 0.0 |> Basics.min 4.0
                newModel =
                    { model | time = time, rageGuy = rageGuy, angerFlash = angerFlash }

                timePlusSeconds seconds =
                    Time.millisToPosix (Time.posixToMillis time + (seconds * 1000))
            in
            if Time.posixToMillis time > Time.posixToMillis model.nextFetchMessages then
                ( { newModel | nextFetchMessages = timePlusSeconds 30 }
                , Cmd.map DiscussionMsg (Discussion.fetchMessagesCmd {})
                )

            else
                ( newModel, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

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

                result =
                    if rageGuy.targetAnger >= 1.0 && not rageGuy.isRaging && not (String.isEmpty model.message) then
                        -- Start raging, post the message
                        let
                            user =
                                model.username

                            newMessage =
                                Discussion.Message user model.time model.topic model.message

                            newModel =
                                { model
                                    | topic = ""
                                    , message = ""
                                    , rageGuy = { rageGuy | thisMoodClock = 0, isRaging = True }
                                }
                        in
                        ( newModel, Cmd.map DiscussionMsg (Discussion.postMessageCmd newMessage) )

                    else if rageGuy.isRaging && rageGuy.thisMoodClock > 50 then
                        -- Cool down
                        ( { model | angerFlash = 5.0, rageGuy = { rageGuy | targetAnger = 0.0, isRaging = False, mood = RageGuy.Neutral } }
                        , Cmd.none
                        )

                    else
                        -- Increment anger and flash
                        ( { model
                            | angerFlash = model.angerFlash + 1.0
                            , rageGuy = RageGuy.update (RageGuy.RageUp 0.05) model.rageGuy
                          }
                        , Cmd.none
                        )
            in
            result

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
                ( { model | username = newUser }, Cmd.none )

            else
                ( model, Cmd.none )

        DiscussionMsg (Discussion.MessagePosted (Ok message)) ->
            ( { model | discussion = message :: model.discussion }
            , Cmd.none
            )

        DiscussionMsg (Discussion.MessagePosted (Err error)) ->
            ( { model | discussion = restDbError "Error while posting message" error model.time :: model.discussion }
            , Cmd.none
            )

        DiscussionMsg (Discussion.MessagesList (Ok messages)) ->
            ( { model | discussion = messages }
            , Cmd.none
            )

        DiscussionMsg (Discussion.MessagesList (Err error)) ->
            ( { model | discussion = restDbError "Error while retrieving messages list" error model.time :: model.discussion }
            , Cmd.none
            )



-- VIEW


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
                        model.angerFlash

                    else if not model.rageGuy.isRaging then
                        -- Click to rage
                        (1.5 + sin (now / 500.0))
                            + (0.5 * sin (0.3 + now / 400.0))
                            + (0.4 * cos (0.8 + now / 300.0))
                            + model.angerFlash

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
                    [ text "Frustrated? Hatin' it? Roaring Rage?" ]
                ]

        usernameInput =
            input
                [ A.type_ "Text"
                , A.placeholder "Your  name"
                , A.value model.username
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

        zeroPad2 x =
            if x < 10 then
                "0" ++ String.fromInt x

            else
                String.fromInt x

        monthToString month =
            case month of
                Time.Jan ->
                    "Jan"

                Time.Feb ->
                    "Feb"

                Time.Mar ->
                    "Mar"

                Time.Apr ->
                    "Apr"

                Time.May ->
                    "May"

                Time.Jun ->
                    "Jun"

                Time.Jul ->
                    "Jul"

                Time.Aug ->
                    "Aug"

                Time.Sep ->
                    "Sep"

                Time.Oct ->
                    "Oct"

                Time.Nov ->
                    "Nov"

                Time.Dec ->
                    "Dec"

        formatTime : Time.Zone -> Time.Posix -> String
        formatTime zone time =
            String.fromInt (Time.toDay zone time)
                ++ " "
                ++ (Time.toMonth zone time |> monthToString)
                ++ " "
                ++ String.fromInt (Time.toYear zone time)
                ++ " "
                ++ zeroPad2 (Time.toHour zone time)
                ++ ":"
                ++ zeroPad2 (Time.toMinute zone time)
                ++ ":"
                ++ zeroPad2 (Time.toSecond zone time)

        viewMessage message =
            let
                username =
                    if String.isEmpty message.username then
                        "Anonymous"

                    else
                        message.username
            in
            div [ A.class "message" ]
                [ span [ A.class "username" ] [ text username ]
                , text " "
                , span [ A.class "topic" ] [ text message.topic ]
                , text " "
                , span [ A.class "timestamp" ] [ text (formatTime model.zone message.timestamp) ]
                , p [ A.class "message" ] [ text message.message ]
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
                    [ usernameInput
                    , br [] []
                    , topicInput
                    , br [] []
                    , messageInput
                    ]
                , div [ A.class "column", Html.Events.onClick ClickRageGuy ] [ rageGuyView ]
                ]
            , hr [] []
            , div [] (List.map viewMessage model.discussion)
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick
