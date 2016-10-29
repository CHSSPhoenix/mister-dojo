port module MisterDojo exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String
import Random exposing (..)
import Time exposing (..)


main : Program (Maybe Model)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = (\msg model -> withSetStorage (update msg model))
        , subscriptions = subscriptions
        }


port setStorage : Model -> Cmd msg


port focus : String -> Cmd msg


withSetStorage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withSetStorage ( model, cmds ) =
    ( model, Cmd.batch [ setStorage model, cmds ] )



-- MODEL


type alias Model =
    { uid : Int
    , viewMode : String
    , temporalDev : Developer
    , devList : List Developer
    , idModerator : Maybe Int
    , roundNumber : Int
    , randomValue : Int
    , minutesCounter : Int
    , isThereARoundActive : Bool
    , currentPlayers : ( Maybe Int, Maybe Int )
    , devPlayedList : List Int
    , playMusic : Bool
    }


type alias Developer =
    { id : Int
    , name : String
    , isBeginner : Bool
    }


defaultDeveloper : Developer
defaultDeveloper =
    { id = 0, name = "", isBeginner = False }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
    ( Maybe.withDefault initialModel savedModel, Cmd.none )


initialModel : Model
initialModel =
    { uid = 0
    , viewMode = "WelcomeMode"
    , temporalDev = defaultDeveloper
    , devList = []
    , idModerator = Nothing
    , roundNumber = 1
    , randomValue = 0
    , minutesCounter = 0
    , isThereARoundActive = False
    , currentPlayers = ( Nothing, Nothing )
    , devPlayedList = []
    , playMusic = False
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        launchAt min =
            if min == 6 then
                RoundCompleted
            else
                IncreaseMinuteCounter
    in
        if model.isThereARoundActive then
            Time.every minute (\_ -> launchAt model.minutesCounter)
        else
            Sub.none



-- UPDATE


type Msg
    = ShowAttendiesView
    | ShowCondingDojoView
    | NoOp
    | AddDeveloper
    | SetModerator Int
    | DeleteDeveloper Int
    | UpdateDevName String
    | UpdateDevLevel
    | GenerateRandom
    | RandomValue Int
    | RoundCompleted
    | IncreaseMinuteCounter
    | StartRound (Maybe Int) (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetModerator id ->
            ( { model | idModerator = Just id }, Cmd.none )

        StartRound id1 id2 ->
            ( { model
                | isThereARoundActive = True
                , playMusic = False
                , currentPlayers = ( id1, id2 )
              }
            , Cmd.none
            )

        RoundCompleted ->
            ( { model
                | minutesCounter = 0
                , isThereARoundActive = False
                , roundNumber = model.roundNumber + 1
                , playMusic = True
                , devPlayedList =
                    case fst model.currentPlayers of
                        Just devId ->
                            (::) devId model.devPlayedList

                        Nothing ->
                            model.devPlayedList
              }
            , Cmd.none
            )

        IncreaseMinuteCounter ->
            ( { model | minutesCounter = model.minutesCounter + 1 }, Cmd.none )

        GenerateRandom ->
            let
                modelPrime =
                    { model
                        | devPlayedList =
                            let
                                myList =
                                    getFilteredList model
                            in
                                if (List.length myList) == 0 then
                                    []
                                else
                                    model.devPlayedList
                    }
            in
                ( modelPrime, Random.generate RandomValue (generateRandomValue modelPrime) )

        RandomValue value ->
            ( { model | randomValue = value }, Cmd.none )

        UpdateDevName str ->
            ( { model
                | temporalDev =
                    { defaultDeveloper
                        | name = str
                        , isBeginner = model.temporalDev.isBeginner
                    }
              }
            , Cmd.none
            )

        UpdateDevLevel ->
            ( { model
                | temporalDev =
                    { defaultDeveloper
                        | name = model.temporalDev.name
                        , isBeginner = not model.temporalDev.isBeginner
                    }
              }
            , Cmd.none
            )

        ShowCondingDojoView ->
            ( { model | viewMode = "CodingDojoMode" }, Cmd.none )

        ShowAttendiesView ->
            ( { model | viewMode = "AttendiesMode", playMusic = False }, Cmd.none )

        AddDeveloper ->
            ( { model
                | uid = model.uid + 1
                , temporalDev = defaultDeveloper
                , devList =
                    if String.isEmpty model.temporalDev.name then
                        model.devList
                    else
                        let
                            newDev =
                                { name = model.temporalDev.name
                                , isBeginner = model.temporalDev.isBeginner
                                , id = model.uid
                                }
                        in
                            (::) newDev model.devList
              }
            , Cmd.none
            )

        DeleteDeveloper id ->
            ( { model | devList = List.filter (\dev -> dev.id /= id) model.devList }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.viewMode of
        "WelcomeMode" ->
            div
                []
                [ section
                    [ id "mainApp" ]
                    [ h1 [] [ text "Mister Dojo - Participants" ]
                    , showActions
                    ]
                , infoFooter
                ]

        "AttendiesMode" ->
            div
                []
                [ section
                    [ id "mainSection" ]
                    [ lazy entry model.temporalDev
                    , lazy developerList model.devList
                    , lazy2 moderator model.idModerator model.devList
                    , showAttendiesActions model
                    ]
                , infoFooter
                ]

        "CodingDojoMode" ->
            let
                ( firstDev, secondDev ) =
                    if model.isThereARoundActive then
                        ( getDeveloperFromId (fst model.currentPlayers) model.devList
                        , getDeveloperFromId (snd model.currentPlayers) model.devList
                        )
                    else
                        getRamdomPlayer model
            in
                div
                    []
                    [ section
                        [ id "codingDojoSection" ]
                        [ h3 [] [ text ("Round Nro. " ++ (toString model.roundNumber)) ]
                        , h4 [] [ text "Players" ]
                        , showRamdomPlayers firstDev secondDev model
                        , showDojoRoundActions firstDev secondDev model
                        , showRoundInfo model
                        , playCompleteRoundMusic model.playMusic
                        ]
                    , infoFooter
                    ]

        _ ->
            div [] []


playCompleteRoundMusic : Bool -> Html e
playCompleteRoundMusic b =
    if b then
        div []
            [ label []
                [ text "Complete round music: " ]
            , audio
                [ src "./audio/SuperMarioBros.mp3"
                , controls True
                , autoplay True
                ]
                []
            ]
    else
        div [] []


moderator : Maybe Int -> List Developer -> Html Msg
moderator id list =
    div []
        [ label [] [ text "Moderator: " ]
        , select
            [ on "change" (Json.map SetModerator targetValueIntDecoder) ]
            (List.map devOption list)
        ]


targetValueIntDecoder : Json.Decoder Int
targetValueIntDecoder =
    targetValue
        `Json.andThen`
            \val ->
                case String.toInt val of
                    Ok i ->
                        Json.succeed i

                    Err err ->
                        Json.fail err


devOption : Developer -> Html e
devOption dev =
    option [ value (toString dev.id) ] [ text dev.name ]


showRoundInfo : Model -> Html e
showRoundInfo model =
    if model.isThereARoundActive then
        div [] [ text (toString model.minutesCounter ++ " minutes") ]
    else
        div [] []


generateRandomValue : Model -> Generator Int
generateRandomValue model =
    let
        init =
            0

        filteredList =
            getFilteredList model

        end =
            let
                total =
                    List.length filteredList
            in
                if total == 0 then
                    0
                else
                    total - 1
    in
        Random.int init end


showRamdomPlayers : Maybe Developer -> Maybe Developer -> Model -> Html e
showRamdomPlayers dev1 dev2 model =
    ul []
        [ li [] [ text (showDevName dev1) ]
        , li [] [ text (showDevName dev2) ]
        ]


showDevName : Maybe Developer -> String
showDevName maybeDev =
    case maybeDev of
        Just dev ->
            dev.name ++ " - Is Beginner? " ++ (toString dev.isBeginner)

        Nothing ->
            ""


getRamdomPlayer : Model -> ( Maybe Developer, Maybe Developer )
getRamdomPlayer model =
    let
        dev1 =
            chooseFirstDev model

        dev2 =
            chooseSecondDev model
    in
        ( dev1, dev2 )


chooseFirstDev : Model -> Maybe Developer
chooseFirstDev model =
    if model.roundNumber == 1 then
        case model.idModerator of
            Just code ->
                getElement code model.devList

            Nothing ->
                Nothing
    else
        getDeveloperFromId (snd model.currentPlayers) model.devList


getElement : Int -> List Developer -> Maybe Developer
getElement id devList =
    List.head (List.filter (\dev -> dev.id == id) devList)


getDeveloperFromId : Maybe Int -> List Developer -> Maybe Developer
getDeveloperFromId maybeId devList =
    case maybeId of
        Just devId ->
            getElement devId devList

        Nothing ->
            Nothing


getFilteredList : Model -> List Developer
getFilteredList model =
    let
        maybeDev1 =
            chooseFirstDev model
    in
        case maybeDev1 of
            Just dev1 ->
                List.filter (\devN -> devN.id /= dev1.id) model.devList
                    |> List.filter (\dev -> not (List.member dev.id model.devPlayedList))

            Nothing ->
                []


chooseSecondDev : Model -> Maybe Developer
chooseSecondDev model =
    let
        secondFilter =
            getFilteredList model
    in
        List.head <| List.drop model.randomValue secondFilter


onEnter : msg -> msg -> Attribute msg
onEnter fail success =
    let
        tagger code =
            if code == 13 then
                success
            else
                fail
    in
        on "keyup" (Json.map tagger keyCode)


entry : Developer -> Html Msg
entry dev =
    header
        []
        [ h1 [] [ text "Mister Dojo - Participants" ]
        , input
            [ id "name"
            , placeholder "Name of the participant?"
            , autofocus True
            , value dev.name
            , name "name"
            , on "input" (Json.map UpdateDevName targetValue)
            , onEnter NoOp AddDeveloper
            ]
            []
        , checkbox UpdateDevLevel "Is Beginner?" dev.isBeginner
        ]


checkbox : msg -> String -> Bool -> Html msg
checkbox msg name isBeginner =
    label
        [ style [ ( "padding", "20px" ) ] ]
        [ input [ type' "checkbox", onClick msg, checked isBeginner ] []
        , text name
        ]


developerList : List Developer -> Html Msg
developerList devList =
    section
        []
        [ ul [] (List.map showDev devList) ]


showDev : Developer -> Html Msg
showDev developer =
    li
        []
        [ div
            []
            [ label [] [ text developer.name ]
            , label [] [ text (" - Is Beginner? " ++ (toString developer.isBeginner)) ]
            , button
                [ class "delete"
                , onClick (DeleteDeveloper developer.id)
                ]
                [ text "Delete" ]
            ]
        ]


showDojoRoundActions : Maybe Developer -> Maybe Developer -> Model -> Html Msg
showDojoRoundActions dev1 dev2 model =
    let
        getId dev =
            dev.id

        hasValue m =
            case m of
                Just _ ->
                    True

                Nothing ->
                    False

        isButtonEnabled =
            (hasValue dev1) && (hasValue dev2)
    in
        if model.isThereARoundActive then
            div [] []
        else
            div []
                [ button [ onClick GenerateRandom ] [ text "Generate again" ]
                , button [ onClick ShowAttendiesView ] [ text "Attendies" ]
                , button
                    [ onClick (StartRound (Maybe.map getId dev1) (Maybe.map getId dev2))
                    , disabled (not isButtonEnabled)
                    ]
                    [ text "Start coding dojo round" ]
                ]


showAttendiesActions : Model -> Html Msg
showAttendiesActions model =
    div []
        [ startCodingDojoButton
        ]


startCodingDojoButton : Html Msg
startCodingDojoButton =
    button
        [ onClick ShowCondingDojoView
        ]
        [ text "Go to Coding Dojo Round"
        ]


showActions : Html Msg
showActions =
    div []
        [ button [ onClick ShowAttendiesView ] [ text "Attendies" ]
        , startCodingDojoButton
        ]


infoFooter : Html msg
infoFooter =
    footer []
        [ p []
            [ text "Written by "
            , a [ href "https://github.com/CHSSPhoenix/mister-dojo.git" ] [ text "Carlos Gómez" ]
            ]
        ]
