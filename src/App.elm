module App exposing (..)

import Command exposing (Config, File(..), catDaemon, fileConfig, stringConfig, updateDaemons, execute)
import FileSystem exposing (Daemon(..), FileSystem, addFile, emptySystem, files, unsafeAddFile)
import Html exposing (br, button, div, text)
import Keyboard exposing (KeyCode)
import Terminal exposing (..)
import Time exposing (Time)


config : Config File
config =
    fileConfig


type alias Model =
    Terminal File


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = (\a b -> (update a b) ! [])
        , view = view config
        , subscriptions = subscriptions
        }


init : Model
init =
    let
        f name =
            unsafeAddFile name (String.toUpper name |> config.fromString)

        system =
            List.foldl f emptySystem files

        daemons =
            [-- { daemon = catDaemon fileConfig "/usr/litter" (Stream "lo"), name = "kitty", lifetime = 100 }
            ]

        prehistory =
            [ "echo \"echo lo ! append hello ! echo \"\"\" | spawn 99 kitty"
            , "daemons"
            , "ls"
            ]

        buffer =
            "echo cat | compile exe | write say | apply hello"

        -- "echo \"echo 4\" | spawn 99 anon"
        -- "echo \"echo 4\" | compile anon | write exec"
        -- "echo \"cat exec\" | compile anon | write exe"
        --"echo hello | write world"
    in
        { history = []
        , buffer = ""
        , system = system
        }
            |> (\m -> List.foldl update m (List.map Execute prehistory))
            |> (\m -> { m | buffer = buffer })



-- UPDATE


type Msg
    = KeyPress KeyCode
    | KeyDown KeyCode
    | Tick Time
    | Execute String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick time ->
            let
                ( streams, system, _ ) =
                    updateDaemons 1 model.system model.system.daemons

                history =
                    (printDaemonOutput config streams) ++ model.history
            in
                ({ model | system = system, history = history })

        KeyPress key ->
            { model | buffer = updateBuffer key model.buffer }

        KeyDown key ->
            { model | buffer = updateBufferDown key model.buffer (List.map (mapEntry config.toString) model.history) }

        Execute buffer ->
            (executeBuffer { model | buffer = buffer })


mapEntry : (a -> b) -> Entry a -> Entry b
mapEntry f entry =
    case entry of
        Input s ->
            Input (f s)

        Output s ->
            Output (f s)


executeBuffer : Model -> Model
executeBuffer model =
    let
        ( stdout, system ) =
            execute config model.buffer model.system

        history =
            Output stdout :: Input (config.fromString model.buffer) :: model.history
    in
        { model | history = history, system = system, buffer = "" }



-- SUBSCRIPTIONS


downSub : { u | buffer : String } -> KeyCode -> Msg
downSub { buffer } key =
    case key of
        8 ->
            KeyPress 8

        13 ->
            Execute buffer

        x ->
            KeyDown x


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Keyboard.downs (downSub model)
    , Keyboard.presses KeyPress
    , Time.every Time.second Tick
    ]
        |> Sub.batch
